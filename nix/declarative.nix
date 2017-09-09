{ nixpkgs, prsJSON }:

with { pkgs = import nixpkgs {}; };

with {
  mkJobset = (
    { enabled          ? 1,
      hidden           ? false,
      description      ? "",
      checkinterval    ? 30,
      schedulingshares ? 10,
      enableemail      ? false,
      emailoverride    ? false,
      keepnr           ? 3,
      nixexprinput,
      nixexprpath,
      inputs
    }@args:

    {
      inherit enabled hidden description;
      inherit emailoverride enableemail;
      inherit checkinterval schedulingshares keepnr;
      inherit nixexprinput nixexprpath inputs;
    } // args);

  mkFetchGithub = value: {
    inherit value;
    type = "git";
    emailresponsible = false;
  };
};

with pkgs.lib;

with rec {
  mkProjectJobset = (
    { url ? null, branch ? "master", ... }@args:

    mkJobset ((removeAttrs args ["url" "branch"]) // {
      nixexprpath = "release.nix";
      nixexprinput = "src";
      inputs = {
        src = mkFetchGithub (
          if url != null
          then "${url} ${branch}"
          else "https://github.com/zenhack/haskell-capnp ${branch}");
      };
    }));

  primaryJobsets = {
    "zenhack-master" = mkProjectJobset {
      branch = "master";
      description = "zenhack/haskell-capnp master";
    };
  };

  prData = builtins.fromJSON (builtins.readFile prsJSON);

  makePr = num: info: (
    with { inherit (info.head) repo; };
    {
      name = "zenhack-master-pr-${num}";
      value = mkProjectJobset {
        description = "PR ${num}: ${info.title}";
        url = "https://github.com/${repo.owner.login}/${repo.name}.git";
        branch = info.head.ref;
      };
    });

  pullRequests = listToAttrs (mapAttrsToList makePr prData);

  jobsetsAttrs = pullRequests // primaryJobsets;
};

{
  jobsets = pkgs.writeText "spec.json" (builtins.toJSON jobsetsAttrs);
}

