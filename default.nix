# Nix shell support is added, but most of us do not use Nix. So GNU
# Guix is preferred if you can choose.

{ pkgs ? import <nixpkgs> { }, }:

pkgs.mkShell {
  packages =
    [ pkgs.emacs pkgs.mupdf-headless pkgs.gnumake pkgs.clang-tools pkgs.gcc ];
}
