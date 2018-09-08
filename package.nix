{ mkDerivation, base, bytestring, hindent, microlens-platform, mtl
, stdenv, text, yi-core, yi-frontend-vty, yi-fuzzy-open
, yi-keymap-vim, yi-misc-modes, yi-mode-haskell, yi-mode-javascript
, yi-rope
}:
mkDerivation {
  pname = "editor";
  version = "0.0.1";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base bytestring hindent microlens-platform mtl text yi-core
    yi-frontend-vty yi-fuzzy-open yi-keymap-vim yi-misc-modes
    yi-mode-haskell yi-mode-javascript yi-rope
  ];
  license = stdenv.lib.licenses.gpl2;
}
