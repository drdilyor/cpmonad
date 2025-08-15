{
  # This is a template created by `hix init`
  inputs.nixpkgs.url = "github:nixos/nixpkgs?ref=nixpkgs-unstable";
  outputs = { self, nixpkgs }:
    let
      system = "x86_64-linux";
      pkgs = import nixpkgs { inherit system; };
    in
    {
      devShells.${system}.default = pkgs.mkShell {
        packages = with pkgs; [
          coreutils
          (haskellPackages.ghcWithPackages (pkgs: with pkgs; [ cabal-install hspec-discover ]))
          ghcid
          haskell-language-server
        ];
      };
    };
}
