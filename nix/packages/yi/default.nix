{ lib
, fetchFromGitHub
, ...
}:
self:
super:
let
  yi-source = fetchFromGitHub {
    owner = "yi-editor";
    repo = "yi";
    rev = lib.fileContents ./rev;
    sha256 = lib.fileContents  ./sha;
  };
in
  {
    yi-language = self.callPackage (self.haskellSrc2nix {
      name = "yi-language";
      src = "${yi-source}/yi-language";
    }) {};

    yi-core = self.callPackage (self.haskellSrc2nix {
      name = "yi-core";
      src = "${yi-source}/yi-core";
    }) {};

    yi-frontend-vty = self.callPackage (self.haskellSrc2nix {
      name = "yi-frontend-vty";
      src = "${yi-source}/yi-frontend-vty";
    }) {};

    yi-keymap-vim = self.callPackage (self.haskellSrc2nix {
      name = "yi-keymap-vim";
      src = "${yi-source}/yi-keymap-vim";
    }) {};

    yi-misc-modes = self.callPackage (self.haskellSrc2nix {
      name = "yi-misc-modes";
      src = "${yi-source}/yi-misc-modes";
    }) {};

    yi-mode-haskell = self.callPackage (self.haskellSrc2nix {
      name = "yi-mode-haskell";
      src = "${yi-source}/yi-mode-haskell";
    }) {};

    yi-mode-javascript = self.callPackage (self.haskellSrc2nix {
      name = "yi-mode-javascript";
      src = "${yi-source}/yi-mode-javascript";
    }) {};

    yi-fuzzy = self.callPackage (self.haskellSrc2nix {
      name = "yi-fuzzy";
      src = "${yi-source}/yi-fuzzy";
    }) {};
  }
