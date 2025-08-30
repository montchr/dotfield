{
  config,
  flake,
  pkgs,
  ...
}:
let
  inherit (flake.perSystem) packages;
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

  programs.git.extraConfig.github.user = whoami.github;

  programs.gh.enable = true;
  programs.gh.settings.git_protocol = "ssh";
}
