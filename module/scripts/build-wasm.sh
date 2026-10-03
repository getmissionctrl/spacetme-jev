#!/usr/bin/env bash
# Build the jev module to a WASI-free SpacetimeDB wasm reactor.
#
# Copy of hs-spacetime's quickstart-chat build-wasm.sh, targeting `jev-module`:
#   1. build   — cross-compile exe:jev-module to wasm via wasm32-wasi-cabal.
#   2. wizer   — snapshot the post-hs_init heap (RTS init WASI calls happen
#                HERE, at snapshot time, with a real WASI shim).
#   3. stub    — compile server/cbits/wasi_stubs.c to a wasm module exporting
#                the 18 WASI funcs + importing memory, then wasm-merge it over
#                the wizened module so residual WASI imports resolve to stubs.
#   4. check   — assert ZERO wasi_snapshot_preview1 imports remain.
#
# Run from this repo inside hs-spacetime's wasm shell, with hs-spacetime
# checked out next to this repo (cabal.project.wasm points at ../hs-spacetime):
#   nix develop ../hs-spacetime#wasm --command module/scripts/build-wasm.sh
# Artifacts land in module/dist/ (gitignored).
set -euo pipefail

root="$(cd "$(dirname "$0")/../.." && pwd)"
hs_spacetime="$root/../hs-spacetime"
dist="$root/module/dist"
mkdir -p "$dist"
cd "$root"

exe="jev-module"

# 1. Build the wasm reactor.
wasm32-wasi-cabal --project-file=cabal.project.wasm build -fwasm "exe:$exe"
built="$(find dist-newstyle -name "$exe.wasm" -type f | head -1)"
[ -n "$built" ] || { echo "no wasm produced" >&2; exit 1; }
cp "$built" "$dist/$exe.wasm"
echo "built $dist/$exe.wasm"

# 2. Wizer snapshot the post-hs_init heap.
wizer \
  --allow-wasi \
  --init-func _initialize \
  --wasm-bulk-memory true \
  "$dist/$exe.wasm" -o "$dist/$exe.wizened.wasm"
echo "wizened -> $dist/$exe.wizened.wasm"

# 3a. Compile the WASI stub module (freestanding, imports memory).
wasm32-wasi-clang -O2 -nostdlib \
  -Wl,--no-entry \
  -Wl,--export-dynamic \
  -Wl,--import-memory \
  "$hs_spacetime/server/cbits/wasi_stubs.c" -o "$dist/$exe.stubs.wasm"

# 3b. Merge: resolve WASI imports against stub exports, unify memory.
wasm-merge \
  "$dist/$exe.wizened.wasm" env \
  "$dist/$exe.stubs.wasm" wasi_snapshot_preview1 \
  --enable-bulk-memory \
  -o "$dist/$exe.nowasi.wasm"
echo "wrote $dist/$exe.nowasi.wasm"

# 4. Assert zero WASI imports remain.
count="$(wasm-tools print "$dist/$exe.nowasi.wasm" | grep -c 'import "wasi_snapshot_preview1"' || true)"
if [ "$count" -ne 0 ]; then
  echo "FAIL: $dist/$exe.nowasi.wasm has $count wasi_snapshot_preview1 import(s)" >&2
  wasm-tools print "$dist/$exe.nowasi.wasm" | grep 'import "wasi_snapshot_preview1"' >&2
  exit 1
fi
echo "OK: $dist/$exe.nowasi.wasm has zero wasi_snapshot_preview1 imports"
