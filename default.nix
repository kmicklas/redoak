with import ./common.nix;

nixpkgs.haskell.packages.ghcjs.callPackage (dynamicCabal2nix (nixpkgs.fetchgitLocal ./.)) {}
