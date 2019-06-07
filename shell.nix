{ pkgs ? import <nixpkgs> {} }:
with pkgs; mkShell {
    name = "Haskell";
    buildInputs = [
        (haskell.packages.ghc865.ghcWithPackages (pkgs: [
            pkgs.hindent
            pkgs.hlint
            pkgs.hoogle
            pkgs.HUnit
            pkgs.matrix
            pkgs.random
            pkgs.tf-random
        ]))
    ];
    shellHook = ''
        . .shellhook
    '';
}
