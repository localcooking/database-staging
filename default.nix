let
  config = {
    packageOverrides = pkgs: rec {
      haskellPackages = pkgs.haskellPackages.override {
        overrides = haskellPackagesNew: haskellPackagesOld: rec {
          operations = haskellPackagesNew.callPackage ../operations/operations.nix {};
        };
      };
    };
  };
in
{ nixpkgs ? import <nixpkgs> { inherit config; }, compiler ? "ghc8104" }:
nixpkgs.pkgs.haskell.packages.${compiler}.callPackage ./staging.nix {}
