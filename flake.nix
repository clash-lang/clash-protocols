{ 
  description = "A flake for the clash-protocols and clash-protocols-base";
  inputs = {
    clash-compiler.url = "github:clash-lang/clash-compiler";
    circuit-notation = {
      url = "github:cchalmers/circuit-notation";
      inputs.clash-compiler.follows = "clash-compiler";
    };
  };
  outputs = { self, flake-utils, clash-compiler, circuit-notation, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        # What version of the GHC compiler to use for protocols
        compiler-version = clash-compiler.ghcVersion.${system};

        pkgs = (import clash-compiler.inputs.nixpkgs {
          inherit system;
        }).extend clash-compiler.overlays.${compiler-version};
        clash-pkgs = pkgs."clashPackages-${compiler-version}";

        overlay = final: prev: {
          # Append the package set with clash-protocols*
          clash-protocols = prev.developPackage {
            root = ./clash-protocols;
            overrides = _: _: final;
          };
          clash-protocols-base = prev.developPackage {
            root = ./clash-protocols-base;
            overrides = _: _: final;
          };
        }
        # Make sure circuit circuit-notation is in scope as well
        // (circuit-notation.overlays.${system}.default final prev);
        hs-pkgs = clash-pkgs.extend overlay;
      in
      {
        # Expose the overlay which adds clash-protocols & clash-protocols-base
        # (...along the inherited overlays)
        # The base of the overlay is clash-pkgs
        overlays.default = overlay;

        devShells.default = hs-pkgs.shellFor {
          packages = p: [
            p.clash-protocols
            p.clash-protocols-base
          ];

          nativeBuildInputs = 
            [
              # Haskell stuff
              hs-pkgs.cabal-install
              hs-pkgs.haskell-language-server
              hs-pkgs.fourmolu
            ]
          ;
        };
        packages = {
          circuit-notation = hs-pkgs.circuit-notation;
          clash-protocols = hs-pkgs.clash-protocols;
          clash-protocols-base = hs-pkgs.clash-protocols-base;

          default = hs-pkgs.clash-protocols;
        };
      });
}
