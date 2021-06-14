# simple.nix
with (import <nixpkgs> {});
mkShell {
  buildInputs = [
    gnumake
    x11
    xorg.libXinerama
    freetype
    yajl
    pkg-config
  ];
}
