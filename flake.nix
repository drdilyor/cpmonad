{
  description = "Competitive programming problemsetting toolchain in Haskell";

  inputs.nixpkgs.url = "github:nixos/nixpkgs?ref=nixpkgs-unstable";
  inputs.flake-parts.url = "github:hercules-ci/flake-parts";

  outputs = inputs@{ flake-parts, nixpkgs, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = [
        "x86_64-linux"
        "aarch64-darwin"
      ];

      perSystem = { system, pkgs, ... }:
        let
          hpkgs = pkgs.haskell.packages.ghc912;

          cpmonad = returnShellEnv:
            hpkgs.developPackage {
              root = ./.;
              inherit returnShellEnv;
              modifier = drv:
                let drv' = 
                  if returnShellEnv then
                    pkgs.haskell.lib.addBuildTools drv (
                      with hpkgs;
                      [
                        cabal-install
                        ghcid
                        haskell-language-server
                      ]
                    )
                  else
                    drv;
                in
                pkgs.haskell.lib.overrideCabal drv' (old: {
                  doCheck = true;
                  # didn't work for some reason
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
