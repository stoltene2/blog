# Find the compiler name by entering the repl and checking
# > nixpkgs = import <nixpkgs> {}
# > nixpkgs.pkgs.haskell.compiler
{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc865" }:
nixpkgs.pkgs.haskell.packages.${compiler}.callPackage ./personal-blog.nix { }
