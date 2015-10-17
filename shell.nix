with import ./common.nix;
let
  inherit (pkgs.haskell.lib) overrideCabal;
  inherit (pkgs.lib.attrsets) mapAttrs;

  f = _: haskellPackages: let
    withSrc = haskellPackages.callPackage
      (dynamicCabal2nix (toString ./.))
      {
        # Hack because cabal2nix includes all conditional deps
        ghcjs-base = haskellPackages.ghcjs-base or null;
      };
    removeSrc = drv: { src = null; };

  in overrideCabal withSrc removeSrc;

  # Make for each package set
  pkgSets = mapAttrs f pkgs.haskell.packages;

  pkgSetsWithCombined = pkgSets // {
    # Single shell for ghc-7.10.2 and ghcjs
    combined = overrideCabal pkgSets.ghc7102 (drv: {
      pname = "${drv.pname}-combined";
      executableHaskellDepends = drv.executableHaskellDepends
        ++ pkgSets.ghcjs.executableHaskellDepends
        ++ [ pkgs.haskellPackages.cabal-install ];
    });
  };

in mapAttrs (_: p: p.env) pkgSetsWithCombined
