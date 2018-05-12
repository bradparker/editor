{ nixpkgs ? import <nixpkgs> {}
, compiler ? "default"
}:
let
  packages = if compiler == "default"
    then nixpkgs.haskellPackages
    else nixpkgs.haskell.packages.${compiler};

  yi-source = nixpkgs.fetchFromGitHub {
    owner = "yi-editor";
    repo = "yi";
    inherit (builtins.fromJSON (builtins.readFile ./yi.json)) rev sha256;
  };
  yi-core = packages.callPackage (packages.haskellSrc2nix {
    name = "yi-core";
    src = "${yi-source}/yi-core";
  }) {};
  yi-frontend-vty = packages.callPackage (packages.haskellSrc2nix {
    name = "yi-frontend-vty";
    src = "${yi-source}/yi-frontend-vty";
  }) {
    inherit yi-core;
  };
  yi-keymap-vim = packages.callPackage (packages.haskellSrc2nix {
    name = "yi-keymap-vim";
    src = "${yi-source}/yi-keymap-vim";
  }) {
    inherit yi-core;
  };
  yi-misc-modes = packages.callPackage (packages.haskellSrc2nix {
    name = "yi-misc-modes";
    src = "${yi-source}/yi-misc-modes";
  }) {
    inherit yi-core;
  };
  yi-mode-haskell = packages.callPackage (packages.haskellSrc2nix {
    name = "yi-mode-haskell";
    src = "${yi-source}/yi-mode-haskell";
  }) {
    inherit yi-core;
  };
in
  packages.callPackage ./package.nix {
    inherit yi-core;
    inherit yi-frontend-vty;
    inherit yi-keymap-vim;
    inherit yi-misc-modes;
    inherit yi-mode-haskell;
  }
