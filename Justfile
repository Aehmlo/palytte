[private]
list:
    @just --list

# Format all source code
format: format-nix format-haskell

# Format Nix source code
format-nix:
    nix fmt

# Format Haskell source code
format-haskell:
    ormolu --mode inplace app/**/*.hs
