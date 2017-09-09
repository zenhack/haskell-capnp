{ nixpkgs ? import ./nix/nixpkgs.nix, compiler ? "default", pkgName ? "capnp" }:

let drv = (import ./release.nix { inherit nixpkgs compiler; }).${pkgName};
in if builtins.getEnv "IN_NIX_SHELL" != "" then drv.env else drv
