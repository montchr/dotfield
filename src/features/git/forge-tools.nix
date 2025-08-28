flake@{ ... }:
{
  aspects.core.home =
    {
      config,
      pkgs,
      ...
    }:
    let
      inherit (flake.config.meta.users.${config.home.username}) whoami;
    in
    {
      home.packages = [
        pkgs.codeberg-cli
        pkgs.gitAndTools.hub
        pkgs.gitAndTools.gh
        pkgs.glab
        pkgs.hut # <- a sourcehut CLI (unofficial)
      ];

      programs.git.extraConfig.github.user = whoami.accounts.github;

      programs.gh.enable = true;
      programs.gh.settings.git_protocol = "ssh";
    };
}
