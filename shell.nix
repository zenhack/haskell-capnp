{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  f = import ./default.nix;

  haskellPackagesNormal = if compiler == "default"
                          then pkgs.haskellPackages
                          else pkgs.haskell.packages.${compiler};

  haskellPackages = haskellPackagesNormal.override {
    overrides = self: super: {
      quota = self.callPackage ./nix/quota.nix {};
    };
  };

  drv = haskellPackages.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
