let
  pkgs = import <nixpkgs> {};
  inherit (pkgs.haskell.lib) overrideCabal;

  dynamicCabal2nix = dir: pkgs.runCommand "dynamic-cabal2nix" {
    nativeBuildInputs = [ pkgs.haskellPackages.cabal2nix ];
  } "cabal2nix ${dir} > $out";

  # TODO: https://github.com/NixOS/cabal2nix/issues/220
  dir = pkgs.runCommand "temp-dir" { } ''
    mkdir -p $out
    cp ${./redoak.cabal} $out
  '';

  generated = dynamicCabal2nix dir;

  extend = _: haskellPackages: haskellPackages.override {
    # packageSetConfig = ...; # TODO: use LTS for stable dep versions
    overrides = self: super: {
      redoak = super.callPackage
        generated
        (# TODO: fix cabal2nix conditional dep support
         if haskellPackages.ghc.isGhcjs or false
         then { glib = null; webkitgtk3 = null; }
         else { });
    };
  };

  redoakPkgs = pkgs.lib.attrsets.mapAttrs extend pkgs.haskell.packages;

  hacked = redoakPkgs // {
    ghcjs = redoakPkgs.ghcjs.override {
      overrides = self: super: {
        # TODO: should be super.redoak
        redoak = overrideCabal redoakPkgs.ghcjs.redoak (drv: {
          src = pkgs.fetchgitLocal ./.;
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
