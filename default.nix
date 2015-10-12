let
  nixpkgs = import <nixpkgs> {};
  dynamicCabal2nix = dir: let
    generated =
      nixpkgs.runCommand "dynamic-cabal2nix" {
        nativeBuildInputs = [ nixpkgs.haskellPackages.cabal2nix ];
      } "cabal2nix ${dir} > $out";

  in import generated;

in nixpkgs.haskell.packages.ghcjs.callPackage (dynamicCabal2nix (nixpkgs.fetchgitLocal ./.)) {}
