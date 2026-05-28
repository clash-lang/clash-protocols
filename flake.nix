{
  description = "A flake for the clash-protocols and clash-protocols-base";
  inputs = {
    clash-compiler.url = "github:clash-lang/clash-compiler";
  };
  outputs = { self, flake-utils, clash-compiler, ... }:
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

            # Each package ships its CHANGELOG.md as a symlink to the repo-root
            # CHANGELOG.md (../CHANGELOG.md). Nix copies the package directory
            # without that out-of-root target, so the symlink dangles in the
            # build sandbox. Cabal >=3.12 (ghc9101+) merely warns about the
            # unmatched 'extra-doc-files' wildcard, but the Cabal 3.10 shipped
            # with ghc96*/ghc98* treats it as a fatal error. Replace the
            # dangling symlink with the real file for those.
            changelog = ./CHANGELOG.md;
            # True for ghc96* / ghc98*, which ship Cabal 3.10.
            uses-cabal-3-10 =
              clash-compiler.inputs.nixpkgs.lib.hasPrefix "ghc96" compiler-version
              || clash-compiler.inputs.nixpkgs.lib.hasPrefix "ghc98" compiler-version;
            fixup-changelog = drv:
              if uses-cabal-3-10 then
                drv.overrideAttrs (pAttr: {
                  postPatch = pAttr.postPatch or "" + ''
                    rm -f CHANGELOG.md
                    cp ${changelog} CHANGELOG.md
                  '';
                })
              else
                drv;

            overlay = final: prev: {
              # The clash package set ships circuit-notation 0.1.0.0, but
              # clash-protocols requires >=0.2. Pull 0.2.0.0 from Hackage.
              circuit-notation = prev.callHackageDirect {
                pkg = "circuit-notation";
                ver = "0.2.0.0";
                sha256 = "sha256-tdM3spbXjQvcnBrmVS0i0tLqoHJ/pnniSOy3eTEZKuw=";
              } {};

              # Append the package set with clash-protocols*
              clash-protocols = fixup-changelog ((prev.developPackage {
                root = ./clash-protocols;
                overrides = _: _: final;
                # Remove me when https://github.com/clash-lang/clash-protocols/issues/131
                # has been solved
                modifier = drv: drv.overrideAttrs (_: { doCheck = false; });
              }).overrideAttrs override-attrs);
              clash-protocols-base = fixup-changelog (prev.developPackage {
                root = ./clash-protocols-base;
                overrides = _: _: final;
              });
            };
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

        minimal-shell = compiler-version: hs-pkgs: hs-pkgs.shellFor {
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
          ]
          # The cabal-gild in the clash package set requires base >=4.19, which
          # ghc96* (base-4.18) doesn't provide, so it can't be built there. We
          # exclude it rather than pin a compatible version, because the last
          # cabal-gild supporting base-4.18 is 1.5.0.0 (everything from 1.5.0.1
          # onwards dropped it), which is too old to be worth carrying for ghc96*.
          ++ clash-compiler.inputs.nixpkgs.lib.optional
               (!clash-compiler.inputs.nixpkgs.lib.hasPrefix "ghc96" compiler-version)
               hs-pkgs.cabal-gild;
        };

        all-shells = clash-compiler.inputs.nixpkgs.lib.attrsets.concatMapAttrs (name: hs-pkgs: {
            # The difference between the `-minimal` and `-full` is the addition of HLS in the full version
            # This is because HLS is slow to compile and not everyone uses it
            # We default to using the `-minimal` version when `nix develop`ing
            "${name}-minimal" = minimal-shell name hs-pkgs;
            "${name}-full" = (minimal-shell name hs-pkgs).overrideAttrs (fAttr: pAttr: {
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
        devShells = all-shells // { default = all-shells."${default-version}-minimal"; };

        # The default directly refers to the default package of the default ghc version of this flake
        # All other entries aren't packages, they're a set of packages for each supported ghc version
        packages = all-packages // { default = all-packages.${default-version}.${default-package}; };
      });
}
