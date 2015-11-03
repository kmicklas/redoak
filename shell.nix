let
  pkgs = import <nixpkgs> {};
  inherit (pkgs.lib.attrsets) mapAttrs;

  addCabalRemoveSrc = _: redoak: pkgs.haskell.lib.overrideCabal redoak (drv: {
    src = null;
    executableHaskellDepends = drv.executableHaskellDepends
      ++ [ pkgs.haskellPackages.cabal-install ];
  });

in mapAttrs (_: p: p.env) (mapAttrs addCabalRemoveSrc (import ./.))
