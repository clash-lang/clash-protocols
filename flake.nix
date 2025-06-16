{ 
  description = "A flake for the clash-protocols and clash-protocols-base";
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    clash-compiler.url = "github:clash-lang/clash-compiler";
  };
  outputs = { nixpkgs, flake-utils, clash-compiler, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        # What version of the GHC compiler to use for protocols
        compiler-version = "ghc910";

        # Sources for things which do not yet have a flake
        non-flake-srcs = {
          circuit-notation = pkgs.fetchFromGitHub {
            owner = "cchalmers";
            repo = "circuit-notation";
            rev = "564769c52aa05b90f81bbc898b7af7087d96613d";
            hash = "sha256-sPfLRjuMxqVRMzXrHRCuKKrdTdqgAJ33pf11DoTP84Q=";
          };
        };

        # Apply aarch64-linux patches
        clash-hs = clash-input-pkgs.haskell // {
          compiler = clash-input-pkgs.haskell.compiler // {
            ${compiler-version} = clash-input-pkgs.haskell.compiler.${compiler-version}.overrideAttrs (prev: {
              patches = let
                  isAarch64 = pkgs.stdenv.hostPlatform.system == "aarch64-linux";
                in
                  (prev.patches or []) ++ pkgs.lib.optional isAarch64 [./nix/aarch64-reloc.patch];
            });
          };
        };

        # Patch programs to be the correct version we want
        overlay = final: prev: {
          clash-ghc = clash-compiler.packages.${system}.clash-ghc;
          clash-prelude = clash-compiler.packages.${system}.clash-prelude;
          clash-prelude-hedgehog = clash-compiler.packages.${system}.clash-prelude-hedgehog;

          string-interpolate = clash-hs.lib.doJailbreak (prev.string-interpolate);

          circuit-notation = final.developPackage {
            root = non-flake-srcs.circuit-notation.outPath;
            overrides = overlay;
          };

          # Packages built by this repository
          clash-protocols = hs-pkgs.developPackage {
            root = ./clash-protocols;
            overrides = overlay;
          };
          clash-protocols-base = hs-pkgs.developPackage {
            root = ./clash-protocols-base;
            overrides = overlay;
          };
        };
        clash-input-pkgs = clash-compiler.inputs.nixpkgs.legacyPackages.${system};
        hs-pkgs = clash-hs.packages.${compiler-version}.extend overlay;

        # General packages from nixpkgs
        pkgs = nixpkgs.legacyPackages.${system};
      in
      {
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
          clash-protocols = hs-pkgs.clash-protocols;
          clash-protocols-base = hs-pkgs.clash-protocols-base;

          default = hs-pkgs.clash-protocols;
        };
      });
}
