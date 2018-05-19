{ mkDerivation, base, microlens-platform, mtl, stdenv, yi-core
, yi-frontend-vty, yi-keymap-vim, yi-misc-modes, yi-mode-haskell
, yi-mode-javascript, yi-rope
}:
mkDerivation {
  pname = "editor";
  version = "0.0.1";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base microlens-platform mtl yi-core yi-frontend-vty yi-keymap-vim
    yi-misc-modes yi-mode-haskell yi-mode-javascript yi-rope
  ];
  license = stdenv.lib.licenses.gpl2;
}
