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
      owner  = "zenhack";
      repo   = "haskell-quota";
      rev    = "534bc2456ab89599093e3dceb1c8e6d6b5d15846";
      sha256 = "1cfdvpdcqy1si6ybq2fsdjmwgdakcyylrn6bw9bg9pahf3xk9n08";
    };

    # Pull in a version of haskell-src-exts that fixes
    # https://github.com/haskell-suite/haskell-src-exts/issues/338, which causes
    # stylish-haskell to choke on some of our template haskell code:
    haskell-src-exts = pkgs.fetchFromGitHub {
      owner  = "mtolly";
      repo   = "haskell-src-exts";
      rev    = "34953c8f8e6d84d2d8b5a8570ee8a145c846fb5d";
      sha256 = "1hrvpw7jp4xm8majsdzn9nhpk1dqghmgvww4a3chikd4f1vdv9ad";
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

  hp = (
    haskellPackagesNormal.override {
      overrides = self: super: (
        with {
          # lots of functions in pkgs.haskell.lib are annoyingly flipped
          addBuildTool = pkgs.lib.flip pkgs.haskell.lib.addBuildTool;
        };

        {
          haskell-src-exts = (
            self.callCabal2nix "haskell-src-exts" sources.haskell-src-exts {});
          haskell-src-meta = self.callHackage "haskell-src-meta" "0.8" {};

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
        hp.stylish-haskell
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

      buildInputs = [ hp.hlint_2_0_5 ];

      phases = ["unpackPhase" "checkPhase"];

      doCheck = true;

      checkPhase = ''
        hlint .
        HLINT_ERROR="$?"
        if [ "$HLINT_ERROR" = 0 ] ; then
            echo "[SUCCESS] hlint report clean"
        else
            echo "[ERROR] hlint has suggestions; please address"
            exit 1
        fi
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
  capnp = addHydraHaddock hp hp.capnp;

  capnpStylish = checkStylishHaskell {
    src = hp.capnp.src;
  };

  capnpHLint = checkHLint { src = hp.capnp.src; };

  capnpCabalNixSync = checkCabalNixSync { src = hp.capnp.src; };
}
