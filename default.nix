with import ./common.nix;
let
  f = _: haskellPackages: haskellPackages.callPackage
    (dynamicCabal2nix (pkgs.fetchgitLocal ./.))
    {
      # Hack because cabal2nix includes all conditional deps
      ghcjs-base = haskellPackages.ghcjs-base or null;
    };

# Make one for each package set
in pkgs.lib.attrsets.mapAttrs f pkgs.haskell.packages
