# spacetime-jev

[pg-jev](https://github.com/realZachi/pg-jev) for SpacetimeDB: plain-language
conditions over a table's rows, answered as calibrated probabilities by a
Jev-compatible decision API (TypeSafe Jev or a local
[Intern-Decision](https://github.com/getmissionctrl/Intern-Decision) server), and
**kept current**: when a row changes, its judgment is redone; when nothing
changed, nothing is sent.

```
source db (e.g. vfview)                       jev db (optional)
  lead ──subscribe──▶ jev-judge ──record_judgment──▶ judgment
                         │  ▲                         condition ──▶ jev-judge
                         ▼  │
                 /v1/jev or /v1/systemone
```

Built on [hs-jev](https://github.com/getmissionctrl/hs-jev) (batched typed
questions) and [hs-spacetime](https://github.com/getmissionctrl/hs-spacetime)
(client + Haskell module authoring).

## How it differs from pg-jev

| | pg-jev | spacetime-jev |
|---|---|---|
| Where it runs | inside Postgres (PL/Python), per query | a sidecar process subscribed to the table |
| Getting rows | read-ahead over TID ranges, guessing what the executor wants next | the subscription pushes them |
| Row → JSON | `to_json(row)` | decoded from the db's own schema (`spacetime describe --json`), no per-table code |
| Cache | per backend session, lost on disconnect | content hash per (condition, row); persisted in the `judgment` table when you use it |
| Results | computed per query | rows anyone can subscribe to; live as data changes |
| Conditions | literals in the query | `--ask`/`--conditions`, or rows in the `condition` table, added live |

Kept from pg-jev: one shared state of rows with one positional question each,
batches capped by row count (pg-jev measured accuracy falling past ~20 on Jev),
in-flight requests, a spend cap.

## Layout

| path | what |
|---|---|
| `src/SpacetimeJev/Dynamic.hs` | schema-driven BSATN → JSON for any table |
| `src/SpacetimeJev/Batch.hs` | one condition × n rows → one `callJev` (`traverse askRow [0..n-1]`) |
| `src/SpacetimeJev/Judge.hs` | the engine: content-hash cache, queue, batching workers, stale-result guard, split-on-reject |
| `src/SpacetimeJev/Sink.hs` | JSON lines to stdout, or reducer calls into the jev module |
| `app/Main.hs` | `jev-judge` CLI |
| `module/` | the jev SpacetimeDB module in Haskell: `condition` + `judgment` tables, wasm reactor |

## Build and test

```sh
nix develop .#dev --command cabal build all
nix develop .#dev --command cabal test all     # hermetic: mock Jev server, fake module backend
```

Against sibling checkouts of hs-jev and hs-spacetime instead of the pinned commits:

```sh
nix develop .#dev --override-input hs-jev path:../hs-jev --override-input hs-spacetime path:../hs-spacetime \
  --command cabal --project-file=cabal.project.dev build all
```

## Trying it on the 3090 against vfview's leads

1. Serve Intern-Decision (in its repo; it listens on `/v1/jev`):

   ```sh
   INFERENCE_CONFIG=configs/inference/default.json MODEL_CHECKPOINT=/path/to/ckpt \
     uvicorn src.service.app:app --host 0.0.0.0 --port 8100
   ```

2. Capture the source schema once:

   ```sh
   spacetime describe --json vfview --server http://127.0.0.1:3000 > vfview.schema.json
   ```

3. Judge the leads and print JSON lines (nothing to publish):

   ```sh
   cabal run jev-judge -- \
     --db vfview --table lead --schema vfview.schema.json \
     --fields research --skip-null \
     --jev-url http://127.0.0.1:8100 --jev-endpoint /v1/jev --model intern-decision-4b \
     --batch 8 --max-chars 12000 --workers 1 \
     --conditions examples/leads.conditions.json \
     --once --with-row > leads.jsonl
   ```

   `--once` judges what is there and exits with stats on stderr (requests,
   rows, tokens, failures). Drop it to keep running and re-judge leads as
   they change. `--key` defaults to the first column; set it if that is not the
   primary key.

   Settings for a local model:
   - **`--batch`**: Intern-Decision rejects requests with more than 16
     questions, so jev-judge clamps to 16 on `/v1/jev`. pg-jev's 20-row result
     was measured on TypeSafe Jev, not on a 4B model. Compare `--batch 1`,
     `4`, `8` and `16` against the same rows before trusting larger batches.
   - **`--max-chars`**: it refuses prompts over 8192 tokens rather than
     truncating. Research write-ups are long, so keep `--max-chars` well under
     ~25k. If a batch is still refused, the judge halves it and retries, down
     to one row.
   - **`--workers`**: the server runs one forward pass at a time behind a
     lock, so more than 1–2 workers only queues requests.

4. Optional: keep judgments in SpacetimeDB.
   - Build and publish the module (needs hs-spacetime's wasm shell):

     ```sh
     nix develop ../hs-spacetime#wasm --command module/scripts/build-wasm.sh
     spacetime publish -b module/dist/jev-module.nowasi.wasm jev
     ```
   - Add conditions as rows. Choice and score options are newline-separated,
     each optionally `name: description`:

     ```sh
     spacetime call jev add_condition '"has-evidence"' '"lead"' \
       '"the research contains real evidence about the company"' '"noul"' '""'
     ```
   - Run jev-judge with `--judgments-db jev` in place of `--conditions`. It
     then:
     - reads its conditions from the `condition` table and picks up new ones live;
     - seeds its cache from existing judgments, so a restart re-sends nothing;
     - writes answers with `record_judgment`.
   - Any client can then subscribe to the results:
     `SELECT * FROM judgment WHERE condition = 'has-evidence' AND value > 0.8`.

## Things to know

- **Data leaves the database** for whatever `--jev-url` points at. That is
  the reason to run Intern-Decision locally.
- **`record_judgment` is open.** Any identity can call it. Before this goes
  anywhere shared, check `sender` against a configured judge identity.
- **Lookups in the module are scans.** The Haskell module ABI has no index
  lookups yet, so every upsert scans the `judgment` table. That is fine for
  thousands of rows, not for millions.
- **Choice options arrive alphabetically.** hs-jev builds choice criteria as
  an Aeson object, which sorts its keys. Answers are mapped back by name, so
  results are correct, but Intern-Decision's README asks for option order to be
  preserved.
- **Memory grows with the table.** The judge holds every current row of the
  source table in memory, so it can backfill a newly added condition.
- **Untested here:** these parts have not been run against a live SpacetimeDB
  or a real Intern-Decision checkpoint:
  - the `jev-judge` wiring;
  - the wasm module build;
  - publishing the module.

  Request bodies were checked against Intern-Decision's own
  `validate_request` and `compile_row`, without a model.
