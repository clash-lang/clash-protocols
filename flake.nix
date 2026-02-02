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

        regular-pkgs = import clash-compiler.inputs.nixpkgs {
          inherit system;
        };

        all-overlays = builtins.listToAttrs (builtins.map (compiler-version:
          let
            # Remove the -fplugin and Setup.hs settings in the .cabal
            # For ghc9101+ these options don't matter, but for ghc964 and ghc982 this breaks installation
            # When entering the installPhase something (I'm not entirely sure what) goes wrong
            # between Nix and GHC, causing Setup.hs to get invoked with the wrong set of packages
            # (I think?). Removing the specific flags during installation fixes the issue for Nix,
            # whilst not breaking regular compilation.
            #
            # Do note that this patch only gets applied during *installation* and not *compilation*
            # That means these flags are still in place during compilation
            override-attrs = if compiler-version == "ghc964" || compiler-version == "ghc982" then
                fAttr: pAttr: {
                  preInstall = pAttr.preInstall or "" + ''
                    sed -i "/-fplugin GHC.TypeLits.Extra.Solver/,+2d" clash-protocols.cabal
                  '';
                }
              else
                {};

            overlay = final: prev: {
              # Append the package set with clash-protocols*
              clash-protocols = (prev.developPackage {
                root = ./clash-protocols;
                overrides = _: _: final;
                # Remove me when https://github.com/clash-lang/clash-protocols/issues/131
                # has been solved
                modifier = drv: drv.overrideAttrs (_: { doCheck = false; });
              }).overrideAttrs override-attrs;
              clash-protocols-base = prev.developPackage {
                root = ./clash-protocols-base;
                overrides = _: _: final;
              };
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

        minimal-shell = hs-pkgs: hs-pkgs.shellFor {
          packages = p: [
            p.clash-protocols
            p.clash-protocols-base
          ];

          # https://discourse.nixos.org/t/non-interactive-bash-errors-from-flake-nix-mkshell/33310
          buildInputs = [
            regular-pkgs.bashInteractive
          ];

          nativeBuildInputs = [
            hs-pkgs.cabal-install
            hs-pkgs.cabal-plan
            hs-pkgs.fourmolu
          ];
        };

        all-shells = clash-compiler.inputs.nixpkgs.lib.attrsets.concatMapAttrs (name: hs-pkgs: {
            # The difference between the `-minimal` and `-full` is the addition of HLS in the full version
            # This is because HLS is slow to compile and not everyone uses it
            # We default to using the `-minimal` version when `nix develop`ing
            "${name}-minimal" = minimal-shell hs-pkgs;
            "${name}-full" = (minimal-shell hs-pkgs).overrideAttrs (fAttr: pAttr: {
              nativeBuildInputs = pAttr.nativeBuildInputs ++ [
                hs-pkgs.haskell-language-server
              ];
            });
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
        #
        # These can be invoked using `nix develop .#ghc9101-minimal`
        #
        # Please do note that if you work with Nix, you need to remove the `cabal.project` file at
        # the root of the directory! Cabal prioritizes local source overrides over Nix, which causes
        # the circuit-notation package to incorrectly fetched from Hackage rather than Nix.
        devShells = all-shells // { default = all-shells."${default-version}-full"; };

        # The default directly refers to the default package of the default ghc version of this flake
        # All other entries aren't packages, they're a set of packages for each supported ghc version
        packages = all-packages // { default = all-packages.${default-version}.${default-package}; };
      });
}
