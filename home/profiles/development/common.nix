{ pkgs, ... }:
{
  imports = [
    ./nix-tools.nix
    ./nodejs.nix
  ];

  home.packages = [
    # TODO: add emacs integration
    pkgs.dotenv-linter # <https://dotenv-linter.github.io/#/?id=dotenv-linter>

    # TODO: disabled because i don't remember why i added it
    # pkgs.nodePackages.terser
  ];
}
