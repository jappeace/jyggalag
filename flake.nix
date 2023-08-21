# I used chatgpt to generate this template and then just
# modified to how I normally use these things.
{
  description = "My Haskell project";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };
  };

  outputs = { self, nixpkgs, flake-compat }:
    let
      pkgs = nixpkgs.legacyPackages.x86_64-linux;
      hpkgs = pkgs.haskellPackages.override {
        overrides = hnew: hold: {
          jyggalag = hnew.callCabal2nix "jyggalag" ./. { };
          # test suite has to tight bounds
          toml-parser = pkgs.haskell.lib.doJailbreak (hold.callHackageDirect {
              pkg = "toml-parser";
              ver = "1.3.0.0";
              sha256 = "sha256-kQZhfMmjGqkHCOJy9IzCDrbDcNWAJUcSDk/S1gpG4r0=";
          } {});
          };
      };
    in
    {
      defaultPackage.x86_64-linux =  hpkgs.jyggalag;
      inherit pkgs;
      devShell.x86_64-linux = hpkgs.shellFor {
        packages = ps : [ ps."jyggalag" ];
        withHoogle = true;

        buildInputs = [
          hpkgs.haskell-language-server
          pkgs.ghcid
          pkgs.cabal-install
        ];
      };
    };
}
