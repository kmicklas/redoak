with import ./env.nix; (import <nixpkgs> {}).lib.mapAttrs (_: p: p.redoak) {
  ghc   = ghc7102;
  ghcjs = ghcjs;
}
