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
        # The package to expose as 'default'
        default-package = "clash-protocols";
        # The 'default' version of ghc to use
        default-version = clash-compiler.ghcVersion.${system};
        # A list of all ghc versions this package supports
        supported-versions = clash-compiler.supportedGhcVersions.${system};

        all-overlays = builtins.listToAttrs (builtins.map (compiler-version:
          let
            # Remove the -fplugin and Setup.hs settings in the .cabal
            # For ghc9101+ these options don't matter, but for ghc964 and ghc982 this breaks compilation
            override-attrs = if compiler-version == "ghc964" || compiler-version == "ghc982" then
                fAttr: pAttr: {
                  preInstall = pAttr.preInstall or "" + ''
                    sed -i "/-fplugin GHC.TypeLits.Extra.Solver/,+2d" clash-protocols*.cabal
                    rm Setup.hs
                  '';
                }
              else
                {};

            overlay = final: prev: {
              # Append the package set with clash-protocols*
              clash-protocols = (prev.developPackage {
                root = ./clash-protocols;
                overrides = _: _: final;
              }).overrideAttrs override-attrs;
              clash-protocols-base = (prev.developPackage {
                root = ./clash-protocols-base;
                overrides = _: _: final;
              }).overrideAttrs override-attrs;
            } // circuit-notation.overlays.${system}.${compiler-version} final prev;
          in
            { name = compiler-version; value = overlay; }
          ) supported-versions);

        all-hs-pkgs = builtins.mapAttrs (compiler-version: overlay:
          let
            pkgs = (import clash-compiler.inputs.nixpkgs {
              inherit system;
            }).extend clash-compiler.overlays.${compiler-version};
            clash-pkgs = pkgs."clashPackages-${compiler-version}";

            hs-pkgs = clash-pkgs.extend overlay;
          in
            hs-pkgs
          ) all-overlays;

        all-shells = builtins.mapAttrs (_: hs-pkgs:
          hs-pkgs.shellFor {
            packages = p: [
              p.clash-protocols
              p.clash-protocols-base
            ];

            nativeBuildInputs =
              [
                # Haskell stuff
                hs-pkgs.cabal-install
                hs-pkgs.cabal-plan
                hs-pkgs.haskell-language-server
                hs-pkgs.fourmolu
              ]
            ;
          }) all-hs-pkgs;

        all-packages = builtins.mapAttrs (_: hs-pkgs:
          {
            clash-protocols = hs-pkgs.clash-protocols;
            clash-protocols-base = hs-pkgs.clash-protocols-base;

            default = hs-pkgs.${default-package};
          }) all-hs-pkgs;
      in
      {
        # Expose the overlay of each supported version which adds clash-protocols(-base)
        # The base of the overlay is clash-pkgs
        overlays = all-overlays // { default = all-overlays.${default-version}; };

        # A devShell for each supported version
        devShells = all-shells // { default = all-shells.${default-version}; };

        # The default directly refers to the default package of the default ghc version of this flake
        # All other entries aren't packages, they're a set of packages for each supported ghc version
        packages = all-packages // { default = all-packages.${default-version}.${default-package}; };
      });
}
