with import ./common.nix;
let
  mkPkg = impl: haskellPackages: let

    fixedHaskellPackages = haskellPackages // (if impl == "ghcjs"
      then { glib = null; webkitgtk3 = null; }
      else { ghcjs-base = null; });

    baseDrv = fixedHaskellPackages.callPackage
      (dynamicCabal2nix (pkgs.fetchgitLocal ./.))
      { };

  in if impl != "ghcjs"
    then baseDrv
    else pkgs.haskell.lib.overrideCabal baseDrv (drv: {
        postInstall = ''
          for jsexe in $out/bin/*.jsexe; do
            cp -r $src/data/* $jsexe
          done
          '';
      });

# Make one for each package set
in pkgs.lib.attrsets.mapAttrs mkPkg pkgs.haskell.packages
