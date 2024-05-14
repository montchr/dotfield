{ pkgs, ... }:
{
  imports = [
    ./nix-tools.nix
    ./nodejs.nix
  ];

  home.packages = [
    # TODO: add as flycheck/flymake check
    pkgs.dotenv-linter # <https://dotenv-linter.github.io/#/?id=dotenv-linter>

    pkgs.nodePackages.prettier

    # TODO: disabled because i don't remember why i added it
    # pkgs.nodePackages.terser
  ];
}
