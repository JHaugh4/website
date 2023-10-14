{ pkgs ? import <nixpkgs> {} }:

with pkgs;

mkShell rec {
    packages = [ haskellPackages.hakyll ];
    buildInputs = [
        zlib
        discord
    ];

    # https://discourse.nixos.org/t/shared-libraries-error-with-cabal-repl-in-nix-shell/8921/9
    # Ensure that libz.so and other libraries are available to TH
    # splices, cabal repl, etc.
    LD_LIBRARY_PATH = lib.makeLibraryPath buildInputs;
}