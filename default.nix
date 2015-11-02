with import ./common.nix;
let
  mkPkg = _: haskellPackages: let
    baseDrv = haskellPackages.callPackage
      (dynamicCabal2nix (pkgs.fetchgitLocal ./.))
      { };
    overrideGhcjs = drv: {
      executableHaskellDepends = drv.executableHaskellDepends
        ++ [ haskellPackages.ghcjs-base ];
      postInstall = ''
        for jsexe in $out/bin/*.jsexe; do
          cp -r $src/data/* $jsexe
        done
        '';
      };

  in
    if builtins.hasAttr "ghcjs-base" haskellPackages
    then pkgs.haskell.lib.overrideCabal baseDrv overrideGhcjs
    else baseDrv;

# Make one for each package set
in pkgs.lib.attrsets.mapAttrs mkPkg pkgs.haskell.packages
