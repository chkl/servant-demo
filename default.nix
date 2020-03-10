{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc865" }:
nixpkgs.pkgs.haskell.lib.dontCheck (nixpkgs.pkgs.haskell.packages.${compiler}.callPackage ./servant-demo.nix { })
