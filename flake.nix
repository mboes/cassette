{
  inputs.nixpkgs.url = "nixpkgs";
  inputs.flake-utils.url = "flake-utils";
  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system: {
      devShells.default =
        let pkgs = import nixpkgs { inherit system; }; in
        pkgs.mkShell {
          packages = with pkgs; [ cabal-install haskell.compiler.ghc9101 ];
      };
    });
}
