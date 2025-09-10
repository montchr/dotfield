{ pkgs, ... }:
{
  imports = [
    #    ./developer.nix

    ../profiles/development/aws.nix
    ../profiles/development/php.nix
    ../profiles/development/work/default.nix
  ];

  home.packages = [
    pkgs.teams-for-linux
  ];
}
