let

reflex-platform = import ((import <nixpkgs> {}).fetchFromGitHub {
  owner = "reflex-frp";
  repo = "reflex-platform";
  rev = "9d846b154bbceb6cc4a6495b3e8d8ea74bb0f7c1";
  sha256 = "0n1m2pbb3qc23abivcr3gw518gkq05mvs71ky4ragp03r5xi0nhs";
}) {};

in

reflex-platform.project ({ pkgs, ... }: {
  packages = {
    redoak = ./.;
  };

  shells = {
    ghc = ["redoak"];
    ghcjs = ["redoak"];
  };

  android = {};
  ios = {};
})
