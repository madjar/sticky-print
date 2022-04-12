build:
    cabal build -w ghc-8.10.7
    cabal build -w ghc-9.0.2
    cabal build -w ghc-9.2.2

example:
    cabal run sticky-print-example -f example