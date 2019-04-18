{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc864" }:
let
  data-prometheus = nixpkgs.haskell.packages.${compiler}.callPackage ./nix/data-prometheus.nix { };
in
nixpkgs.haskell.packages.${compiler}.callPackage ./machine-check.nix { inherit data-prometheus; }
