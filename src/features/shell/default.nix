{ lib', self, ... }:
let
  aliases = {
    # Always enable colored `grep` output
    grep = "grep --color=auto";
    fgrep = "fgrep --color=auto";
    egrep = "egrep --color=auto";
  };
in
{
  dotfield.home =
    { pkgs, ... }:
    {
      imports = [
        (lib'.shell.makeShellAliasesModule { inherit aliases; })
      ];

      environment.shells = [
        pkgs.bashInteractive
        pkgs.fish
      ];

      # Install completions for system packages.
      environment.pathsToLink = [
        "/share/bash-completion"
      ];

      programs.bottom.enable = true;
      programs.eza.enable = true;
      programs.fzf.enable = true;
      programs.htop.enable = true;
      programs.less.enable = true;

      programs.readline = {
        enable = true;
        variables = {
          # Expand tilde to home directory.
          expand-tilde = true;

          # Improve completion usability.
          completion-ignore-case = true;
          completion-map-case = true;

          # Avoid pressing TAB so much.
          show-all-if-ambiguous = true;
          show-all-if-unmodified = true;

          # Indicate file types.
          visible-stats = true;
        };
      };
    };
}
