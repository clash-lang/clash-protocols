{ sources ? import ./sources.nix }:

let
  haskell_compiler = "ghc965";

  overlay = _: pkgs: {

    # Nix tooling
    niv = (import sources.niv {}).niv;
    gitignore = import sources.gitignore { inherit (pkgs) lib; };

    haskell = pkgs.haskell // {
      compiler = pkgs.haskell.compiler // {
        "${haskell_compiler}" = pkgs.haskell.compiler.${haskell_compiler}.overrideAttrs (old: {
          # Fix for linking issues: https://gitlab.haskell.org/ghc/ghc/-/issues/24432
          patches =
           let isAarch64 = pkgs.stdenv.hostPlatform.system == "aarch64-linux";
           in (old.patches or [ ]) ++ pkgs.lib.optional isAarch64 [ ./aarch64-reloc.patch ];
        });
      };
    };

    # Haskell overrides
    haskellPackages = pkgs.haskell.packages.${haskell_compiler}.override {
      overrides = self: super: {
        # Add overrides here
        circuit-notation =
          self.callCabal2nix "circuit-notation" sources.circuit-notation {};
        doctest-parallel =
          self.callCabal2nix "doctest-parallel" sources.doctest-parallel {};
        clash-prelude =
          self.callCabal2nix "clash-prelude" (sources.clash-compiler + "/clash-prelude") {};
        clash-lib =
          self.callCabal2nix "clash-lib" (sources.clash-compiler + "/clash-lib") {};
        clash-ghc =
          self.callCabal2nix "clash-ghc" (sources.clash-compiler + "/clash-ghc") {};
        clash-prelude-hedgehog =
          self.callCabal2nix "clash-prelude" (sources.clash-compiler + "/clash-prelude-hedgehog") {};
        tasty-hedgehog =
          self.callCabal2nix "tasty-hedgehog" sources.tasty-hedgehog {};
        hedgehog =
          self.callCabal2nix "hedgehog" (sources.haskell-hedgehog + "/hedgehog") {};
        clash-protocols-base =
          self.callCabal2nix "clash-protocols-base" (../clash-protocols-base) {};
      };
    };
  };

in import sources.nixpkgs { overlays = [ overlay ]; }
