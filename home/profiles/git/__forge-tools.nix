{
  config,
  flake,
  pkgs,
  ...
}:
let
  inherit (flake.perSystem) packages;
  inherit (config.dotfield) whoami;
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
  programs.gh.extensions = [
    packages.gh-repo-explore
    packages.gh-s

    pkgs.gh-dash
    pkgs.gh-eco
  ];

}
