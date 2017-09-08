# In order to update `nixpkgs.json` to a specific revision, run:
#
# ```bash
# $ nix-prefetch-git https://github.com/NixOS/nixpkgs.git "${REVISION}" > nixpkgs.json
# ```

with rec {
  system = builtins.currentSystem;

  builtin-paths = import <nix/config.nix>;

  nixpkgs-json = builtins.fromJSON (builtins.readFile ./nixpkgs.json);

  # The nixpkgs to use when bootstrapping. This shouldn't matter except insofar
  # as it needs to have fetchFromGitHub and ideally the binary cache should be
  # populated for this.
  hashes = {
    commit = "76d649b59484607901f0c1b8f737d8376a904019";
    sha = "04xp12gmjby84k4pabc0nspggf4pgycnl764zf7w50izx65r9q8x";
  };

  stage1-tarball = import <nix/fetchurl.nix> {
    url = "https://github.com/NixOS/nixpkgs/archive/${hashes.commit}.tar.gz";
    sha256 = hashes.sha;
  };

  stage1-path = builtins.derivation {
    name = "nixpkgs-bootstrap-stage1";
    builder = builtin-paths.shell;
    args = [
      (builtins.toFile "nixpkgs-unpacker" ''
        "$coreutils/mkdir" -p "$out"
        cd "$out"
        "$gzip" -c -d "$tarball" > temporary.tar
        "$tar" -xf temporary.tar --strip-components=1
        "$coreutils/rm" temporary.tar
      '')
    ];

    inherit system;

    inherit (builtin-paths) tar gzip coreutils;
    tarball = stage1-tarball;
  };

  stage1 = import stage1-path { inherit system; };
};

stage1.fetchFromGitHub {
  owner = "NixOS";
  repo  = "nixpkgs";
  inherit (nixpkgs-json) rev sha256;
}
