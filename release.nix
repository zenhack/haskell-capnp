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

  checkStylishHaskell = { src, ignore ? [] }: (
    pkgs.stdenv.mkDerivation {
      name = "check-stylish-haskell";

      inherit src;

      buildInputs = [
        pkgs.git
        pkgs.gnused
        pkgs.haskellPackages.stylish-haskell
      ];

      phases = ["unpackPhase" "checkPhase"];

      doCheck = true;

      checkPhase = ''
        mkdir "$out"

        ${pkgs.lib.concatMapStrings (x: "echo ${x} >> .gitignore\n") ignore}
        git init                                   &> /dev/null
        git config user.email "ignore@example.com" &> /dev/null
        git config user.name  "Ignore Me"          &> /dev/null
        git add '*'                                &> /dev/null
        git commit -m 'temporary'                  &> /dev/null

        for file in $(git ls-files '*.hs'); do
            set +e
            STYLISH_ERROR="$(stylish-haskell -i "$file" |& cat)"
            STYLISH_CODE="$?"
            set -e
            if [[ "$STYLISH_CODE" != "0" ]]; then
                echo "[FAILURE] stylish-haskell failed on $file"
                stylish-haskell -v -i "$file" |& sed 's/^/[FAILURE]     /g'
                exit 1
            fi
        done

        if test -n "$(git diff)"; then
            echo "[FAILURE] stylish-haskell suggestion diff:"
            git diff | sed 's/^/[FAILURE]     /g'
            exit 2
        fi

        echo "[SUCCESS] stylish-haskell had no suggestions"
      '';
    });

  checkHLint = { src }: (
    pkgs.stdenv.mkDerivation {
      name = "check-hlint";

      inherit src;

      buildInputs = [ pkgs.haskellPackages.hlint_2_0_5 ];

      phases = ["unpackPhase" "checkPhase"];

      doCheck = true;

      checkPhase = ''
        test -e report.html && {
            echo "[FAILURE] report.html already exists"; exit 1
        }

        hlint . --report || true

        mkdir -pv "$out/nix-support"
        mv report.html "$out/report.html"
        echo "doc report $out report.html" \
            >> "$out/nix-support/hydra-build-products"

        echo "[SUCCESS] hlint report generated"
      '';
    });

  checkCabalNixSync = { src }: (
    pkgs.stdenv.mkDerivation {
      name = "check-cabal-nix-sync";

      inherit src;

      buildInputs = [
        pkgs.git
        pkgs.gnumake
        pkgs.haskellPackages.cabal2nix
      ];

      phases = ["unpackPhase" "checkPhase"];

      doCheck = true;

      checkPhase = ''
        mkdir "$out"

        git init                                   &> /dev/null
        git config user.email "ignore@example.com" &> /dev/null
        git config user.name  "Ignore Me"          &> /dev/null
        git add '*'                                &> /dev/null
        git commit -m 'temporary'                  &> /dev/null

        make nix/capnp.nix

        if test -n "$(git diff)"; then
            echo "[FAILURE] nix/capnp.nix is out of date!"
            exit 1
        fi

        echo "[SUCCESS] nix/capnp.nix is up to date"
      '';
    });
};

{
  capnp = addHydraHaddock haskellPackages haskellPackages.capnp;

  capnpStylish = checkStylishHaskell {
    src = haskellPackages.capnp.src;
    ignore = [
      "library/Language/CapNProto/TH.hs"
    ];
  };

  capnpHLint = checkHLint { src = haskellPackages.capnp.src; };

  capnpCabalNixSync = checkCabalNixSync { src = haskellPackages.capnp.src; };
}
