{
  inputs = {
    nixpkgs.url = "nixpkgs";
    flake-utils.url = "flake-utils";
    treefmt-nix.url = "github:numtide/treefmt-nix";
    treefmt-nix.inputs.nixpkgs.follows = "nixpkgs";
  };
  outputs =
    {
      self,
      nixpkgs,
      flake-utils,
      treefmt-nix,
    }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = import nixpkgs { inherit system; };
        treefmt = (treefmt-nix.lib.evalModule pkgs ./treefmt.nix).config.build;
      in
      {
        devShells.default = pkgs.mkShell {
          packages =
            with pkgs;
            let
              doctest = lib.pipe haskell.packages.ghc910.doctest [
                (haskell.lib.compose.enableCabalFlag "cabal-doctest")
                haskell.lib.compose.dontCheck
              ];
            in
            [
              cabal-install
              doctest
              haskell.compiler.ghc9101
            ];
        };
        formatter = treefmt.wrapper;
        checks = {
          formatting = treefmt.check self;
        };
      }
    );
}
