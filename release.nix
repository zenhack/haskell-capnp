{ nixpkgs ? import ./nix/nixpkgs.nix, compiler ? "default" }:

with rec {
  pkgs = import nixpkgs {};

  inherit (pkgs) lib;

  haskellPackagesNormal = (
    if compiler == "default"
    then pkgs.haskellPackages
    else pkgs.haskell.packages.${compiler});

  sources = {
    quota = pkgs.fetchFromGitHub {
      repo   = "haskell-quota";
      owner  = "zenhack";
      rev    = "3059085facf116857313a88ffc0debefbf86aafa";
      sha256 = "1xaj4vfa67m674z1h18g7jafpp36c10jjf3jcwvdr0b2a0p6i06c";
    };
  };

  withFilteredSource = pkg: pkg.overrideDerivation (old:
    with {
      sf = name: type: let bn = baseNameOf (toString name); in !(
        (type == "directory" && (bn == ".git"))
        || pkgs.lib.hasSuffix "~" bn
        || pkgs.lib.hasSuffix ".o" bn
        || pkgs.lib.hasSuffix ".so" bn
        || pkgs.lib.hasSuffix ".nix" bn
        || (type == "symlink" && pkgs.lib.hasPrefix "result" bn)
      );
    };
    { src = builtins.filterSource sf ./.; });

  haskellPackages = (
    haskellPackagesNormal.override {
      overrides = self: super: (
        with {
          # lots of functions in pkgs.haskell.lib are annoyingly flipped
          addBuildTool = pkgs.lib.flip pkgs.haskell.lib.addBuildTool;
        };

        {
          quota = self.callCabal2nix "quota" sources.quota {};
          capnp = (
            withFilteredSource
            (addBuildTool pkgs.capnproto
            (self.callPackage ./nix/capnp.nix {})));
        });
    });
};

{
  capnp = haskellPackages.capnp;
}
