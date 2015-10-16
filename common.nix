rec {
  nixpkgs = import <nixpkgs> {};

  dynamicCabal2nix = dir: nixpkgs.runCommand "dynamic-cabal2nix" {
    nativeBuildInputs = [ nixpkgs.haskellPackages.cabal2nix ];
  } "cabal2nix ${dir} > $out"; 

  fullBuild = dir: ghc: ghc.callPackage
    (dynamicCabal2nix "file://${(nixpkgs.fetchgitLocal ./.)}/${dir}")
    {};

  shell = dir: ghc: let
    withSrc = ghc.callPackage (dynamicCabal2nix (toString dir)) {};
    removeSrcAddCabal = drv: {
      src = null;
      executableHaskellDepends =
        drv.executableHaskellDepends ++
        [ nixpkgs.haskellPackages.cabal-install ];
    };
  in (nixpkgs.haskell.lib.overrideCabal withSrc removeSrcAddCabal).env;
}
