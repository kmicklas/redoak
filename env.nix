let
  pkgs = import <nixpkgs> {};
  inherit (pkgs.haskell.lib) overrideCabal;

  dynamicCabal2nix = dir: pkgs.runCommand "dynamic-cabal2nix" {
    nativeBuildInputs = [ pkgs.haskellPackages.cabal2nix ];
  } "cabal2nix ${dir} > $out";

  generated = dynamicCabal2nix (pkgs.fetchgitLocal ./.);

  extend = _: haskellPackages: haskellPackages.override {
    # packageSetConfig = ...; # TODO: use LTS for stable dep versions
    overrides = self: super: {
      bifunctors = overrideCabal super.bifunctors (drv: {
        version = "5.1";
        sha256 = "1p9m8rbrfzh9a4hgllh2hldpc97nxlis97y5biak6g4czs6xxn8b";
        doCheck = false;
      });

      redoak = super.callPackage
        generated
        (# TODO: fix cabal2nix conditional dep support
         if haskellPackages.ghc.isGhcjs or false
         then { glib = null; webkitgtk3 = null; }
         else { });
    };
  };

  readoakPkgs = pkgs.lib.attrsets.mapAttrs extend pkgs.haskell.packages;

  hacked = readoakPkgs // {
    ghcjs = readoakPkgs.ghcjs.override {
      overrides = self: super: {
        # TODO: should be super.redoak
        redoak = overrideCabal readoakPkgs.ghcjs.redoak (drv: {
          # TODO: fix cabal2nix conditional dep support
          executableHaskellDepends = drv.executableHaskellDepends
            ++ [ super.ghcjs-base ];
          # TODO: fix GHCJS data-dir support
          postInstall = ''
            for jsexe in $out/bin/*.jsexe; do
              cp -r $src/data/* $jsexe
            done
          '';
        });
      };
    };
  };

in hacked
