let
  reflexPlatform = import ((import <nixpkgs> {}).fetchFromGitHub {
    owner = "reflex-frp";
    repo = "reflex-platform";
    rev = "3106fa5bee6f737d8f7edf71f3ca190e0549539d";
    sha256 = "1kxd4nmsk4yms037q831zy4mhcmnp4x8i9phk948lp2y5cf73ija";
  }) {};
  pkgs = reflexPlatform.nixpkgs;
  inherit (pkgs.haskell.lib) overrideCabal;

  # TODO: https://github.com/NixOS/cabal2nix/issues/220
  dir = pkgs.runCommand "temp-dir" { } ''
    mkdir -p $out
    cp ${./redoak.cabal} $out
  '';

  generated = reflexPlatform.cabal2nixResult dir;

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

  redoakPkgs = pkgs.lib.attrsets.mapAttrs extend { inherit (reflexPlatform) ghc ghcjs; };

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
