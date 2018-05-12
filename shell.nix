{ nixpkgs ? import <nixpkgs> {}
, compiler ? "default"
}:
let
  packages = if compiler == "default"
    then nixpkgs.haskellPackages
    else nixpkgs.haskell.packages.${compiler};

  hlint = packages.hlint;
  hindent = packages.hindent;
  cabal = packages.cabal-install;

  env = (import ./default.nix { inherit nixpkgs; inherit compiler; }).env;
in
  nixpkgs.lib.overrideDerivation env (drv: {
    nativeBuildInputs = drv.nativeBuildInputs ++ [ hlint hindent cabal ];
  })
