rec {
  nixpkgs = import <nixpkgs> {};
  dynamicCabal2nix = dir: let
    generated =
      nixpkgs.runCommand "dynamic-cabal2nix" {
      nativeBuildInputs = [ nixpkgs.haskellPackages.cabal2nix ];
    } "cabal2nix ${dir} > $out"; 

  in generated;
}
