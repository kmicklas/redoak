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

in mapAttrs (_: p: p.env) (mapAttrs f pkgs.haskell.packages);
