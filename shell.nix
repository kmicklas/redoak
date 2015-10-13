with import ./common.nix;
let
  withSrc = nixpkgs.haskell.packages.ghcjs.callPackage (dynamicCabal2nix (toString ./.)) {};

in (withSrc.overrideDerivation (_: { src = null; })).env
