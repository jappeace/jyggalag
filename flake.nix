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
          poppen = hnew.callCabal2nix "poppen" ./. { };
        };
      };
    in
    {
      defaultPackage.x86_64-linux =  hpkgs.poppen;
      inherit pkgs;
      devShell.x86_64-linux = hpkgs.shellFor {
        packages = ps : [ ps."poppen" ];
        withHoogle = true;

        buildInputs = [
          hpkgs.haskell-language-server
          pkgs.ghcid
          pkgs.cabal-install
        ];
      };
    };
}
