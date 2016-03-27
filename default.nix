(import <nixpkgs> {}).lib.mapAttrs (_: p: p.redoak) (import ./env.nix)
