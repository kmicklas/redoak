rec {
  nixpkgs = import <nixpkgs> {};

  dynamicCabal2nix = dir: flags: nixpkgs.runCommand "dynamic-cabal2nix" {
    nativeBuildInputs = [ nixpkgs.haskellPackages.cabal2nix ];
  } "cabal2nix ${flags} ${dir} > $out";

  fullBuild = ghc: flags:
    ghc.callPackage (dynamicCabal2nix (nixpkgs.fetchgitLocal ./.) flags) {};

  shell = ghc: flags: let
    withSrc = ghc.callPackage (dynamicCabal2nix (toString ./.) flags) {};
    removeSrcAddCabal = drv: {
      src = null;
      executableHaskellDepends =
        drv.executableHaskellDepends ++
        [ nixpkgs.haskellPackages.cabal-install ];
    };
  in (nixpkgs.haskell.lib.overrideCabal withSrc removeSrcAddCabal).env;
}
