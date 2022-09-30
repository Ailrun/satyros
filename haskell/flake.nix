# Maintaining this file:
#
#     - Bump the inputs version using `nix flake update`
#     - Edit `sourceDirs` to update the set of local packages
#
# For more details: https://nixos.wiki/wiki/Flakes
{
  description = "satyros-ui-haskell flake";

  inputs = {
    haskellNix.url = "github:input-output-hk/haskell.nix";
    nixpkgs.follows = "haskellNix/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };
  outputs =
    { self, haskellNix, nixpkgs, flake-utils }:
    flake-utils.lib.eachSystem [ "x86_64-linux" "x86_64-darwin" ] (system:
    let
      overlays = [ haskellNix.overlay
        (final: prev: {
          # This overlay adds our project to pkgs
          satyros-ui-haskell =
            final.haskell-nix.project' {
              src = ./.;
              compiler-nix-name = "ghc865";
              shell = {
                # This is used by `nix develop .` to open a shell for use with
                # `cabal`, `hlint` and `haskell-language-server`
                tools = {
                  cabal = {};
                };
                # This adds `js-unknown-ghcjs-cabal` to the shell.
                crossPlatforms = p: [p.ghcjs];
              };
            };
        })
      ];
      pkgs = import nixpkgs { inherit system overlays; };
      flake = pkgs.satyros-ui-haskell.flake {
        # This adds support for `nix build .#js-unknown-ghcjs:satyros-ui-haskell:exe:satyros-ui-haskell`
        crossPlatforms = p: [p.ghcjs];
      };
    in flake // {
      # Built by `nix build .`
      defaultPackage = flake.packages."satyros-ui-haskell:exe:satyros-ui-haskell";
    });
}
