{
  description = "Competitive programming problemsetting toolchain in Haskell";

  inputs.nixpkgs.url = "github:nixos/nixpkgs?ref=nixpkgs-unstable";
  inputs.flake-parts.url = "github:hercules-ci/flake-parts";

  outputs =
    inputs@{ flake-parts, nixpkgs, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = [
        "x86_64-linux"
        "aarch64-darwin"
      ];

      perSystem =
        { system, pkgs, ... }:
        let
          hpkgs = pkgs.haskell.packages.ghc912;

          cpmonad =
            returnShellEnv:

            hpkgs.developPackage {
              root = ./.;
              inherit returnShellEnv;
              modifier = pkgs.haskell.lib.compose.overrideCabal (old: {
                buildTools =
                  (old.buildTools or [ ])
                  ++ pkgs.lib.optionals returnShellEnv [
                    hpkgs.cabal-install
                    hpkgs.ghcid
                    hpkgs.haskell-language-server
                  ];

                doCheck = true;
                doHaddock = returnShellEnv;
                enableLibraryProfiling = returnShellEnv;
                enableExecutableProfiling = returnShellEnv;

              });
            };
        in
        {
          packages.default = cpmonad false;
          devShells.default = cpmonad true;
        };
    };
}
