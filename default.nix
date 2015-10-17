with import ./common.nix;
let
  mkPkg = _: haskellPackages: let
    withoutCondDep = haskellPackages.callPackage
      (dynamicCabal2nix (pkgs.fetchgitLocal ./.))
      { };
    addCondDeps = drv: {
      executableHaskellDepends = drv.executableHaskellDepends
        ++ [ haskellPackages.ghcjs-base or null ];
      };

  in pkgs.haskell.lib.overrideCabal withoutCondDep addCondDeps;

# Make one for each package set
in pkgs.lib.attrsets.mapAttrs mkPkg pkgs.haskell.packages
