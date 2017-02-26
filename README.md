# redoak
An editor for the future

## Building on GHC with GTK

    nix-shell -A ghc
    > ./configure
    > ./build
    > cabal run

## Building on GHCJS

    nix-shell -A ghcjs
    > ./configure --ghcjs
    > ./build
    > ./serve

Then vist `http://localhost:8000/`.
