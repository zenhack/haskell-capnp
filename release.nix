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
      rev    = "9f0b67cdba226a0cfe23502e044adce48936a1e6";
      sha256 = "0bi50addsnxs4wmbn6ybvjldsi01q9ji7m4d3s7ca9visq3n6gvj";
    };
  };

  computeHaskellDir = hp: pkg: "${pkg.system}-${hp.ghc.name}";

  addHydraHaddock = hp: pkg: (
    with rec {
      suffix = "share/doc/${computeHaskellDir hp pkg}/${pkg.name}/html";
    };

    pkgs.haskell.lib.overrideCabal pkg (old: rec {
      doHaddock = true;
      postInstall = ((old.postInstall or "") + ''
        mkdir -pv "$out/nix-support"
        echo "doc haddock $out/${suffix} index.html" \
            >> "$out/nix-support/hydra-build-products"
      '');
    }));

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
  capnp = addHydraHaddock haskellPackages haskellPackages.capnp;
}
