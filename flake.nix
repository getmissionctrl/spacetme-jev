{
  description = "spacetime-jev — natural-language judgments over SpacetimeDB tables (hs-jev + hs-spacetime)";

  inputs = {
    # Same pin as hs-spacetime, so the GHC package set matches.
    nixpkgs.url = "github:NixOS/nixpkgs/8ce4ef6cb6f871616146b9fe26d2a5ae594e94fe";
    # Sources only: cabal builds them from cabal.project; the flake just supplies their deps.
    hs-jev = { url = "github:getmissionctrl/hs-jev"; flake = false; };
    hs-spacetime = { url = "github:getmissionctrl/hs-spacetime"; flake = false; };
  };

  outputs = { self, nixpkgs, hs-jev, hs-spacetime }:
    let
      systems = [ "x86_64-linux" "aarch64-linux" "aarch64-darwin" "x86_64-darwin" ];
      forAll = f: nixpkgs.lib.genAttrs systems (system: f (import nixpkgs { inherit system; }));
    in {
      devShells = forAll (pkgs:
        let
          hp = pkgs.haskellPackages.override {
            overrides = self: _: {
              hs-jev = self.callCabal2nix "hs-jev" hs-jev {};
              hs-spacetime = self.callCabal2nix "hs-spacetime" hs-spacetime {};
              jev-module = self.callCabal2nix "jev-module" ./module {};
              spacetime-jev = self.callCabal2nix "spacetime-jev" ./. {};
            };
          };
          pkgsToBuild = [ hp.hs-jev hp.hs-spacetime hp.jev-module hp.spacetime-jev ];
          # GHC with every dependency of the four packages (not the packages
          # themselves), so cabal needs no Hackage index.
          dev = hp.shellFor {
            packages = _: pkgsToBuild;
            nativeBuildInputs = [ pkgs.cabal-install pkgs.fourmolu pkgs.brotli pkgs.zlib pkgs.pkg-config ];
          };
        in { dev = dev; default = dev; });
    };
}
