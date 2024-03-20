{ sources ? import ./sources.nix }:

let
  overlay = _: nixpkgs: {

    # Nix tooling
    niv = (import sources.niv {}).niv;
    gitignore = import sources.gitignore { inherit (nixpkgs) lib; };

    # Haskell overrides
    haskellPackages = nixpkgs.haskellPackages.override {
      overrides = self: super: {
        # External overrides
        circuit-notation =
          self.callCabal2nix "circuit-notation" sources.circuit-notation {};

        clash-lib =
          self.callCabal2nix "clash-lib" "${sources.clash-compiler}/clash-lib" {};

        clash-ghc =
          self.callCabal2nix "clash-ghc" "${sources.clash-compiler}/clash-ghc" {};

        clash-prelude =
          self.callCabal2nix "clash-prelude" "${sources.clash-compiler}/clash-prelude" {};
      };
    };
  };

in import sources.nixpkgs { overlays = [ overlay ]; }
