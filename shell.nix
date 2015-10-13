with import ./common.nix;
let
  withSrc = nixpkgs.haskell.packages.ghcjs.callPackage (dynamicCabal2nix (toString ./.)) {};
  removeSrcAddCabal = drv: {
    src = null;
    executableHaskellDepends = drv.executableHaskellDepends ++ [ nixpkgs.haskellPackages.cabal-install ];
  };

in (nixpkgs.haskell.lib.overrideCabal withSrc removeSrcAddCabal).env
