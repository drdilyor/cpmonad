{
  description = "Competitive programming problemsetting toolchain in Haskell";

  inputs.nixpkgs.url = "github:nixos/nixpkgs?ref=nixpkgs-unstable";
  inputs.flake-parts.url = "github:hercules-ci/flake-parts";

  outputs = inputs@{ flake-parts, nixpkgs, ... }: flake-parts.lib.mkFlake { inherit inputs; } {
    systems = [
      "x86_64-linux"
      "aarch64-darwin"
    ];

    perSystem = { system, pkgs, ... }:
      let
        hpkgs = pkgs.haskell.packages.ghc912;
      in
      {
        devShells.default = pkgs.mkShell {
          packages = [
            pkgs.coreutils
            hpkgs.ghc
            hpkgs.cabal-install
            hpkgs.ghcid
            hpkgs.haskell-language-server
          ];
        };
      };
  };
}
