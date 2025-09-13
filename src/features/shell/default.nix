{
  aspects.core = {
    nixos =
      { pkgs, ... }:
      {
        environment.shells = [
          pkgs.bashInteractive
          pkgs.fish
        ];

        # Install completions for system packages.
        environment.pathsToLink = [
          "/share/bash-completion"
        ];
      };

    home =
      { pkgs, ... }:
      {
        programs.bat.enable = true;
        programs.bottom.enable = true;
        programs.carapace.enable = true;
        programs.dircolors.enable = true;
        programs.eza.enable = true;
        programs.fzf.enable = true;
        programs.info.enable = true;
        programs.less.enable = true;
        programs.zellij.enable = true;

        home.packages = [
          pkgs.duf # <- du/df alternative
          pkgs.fx
          pkgs.glow
          pkgs.hexyl
          pkgs.igrep
          pkgs.ouch
        ];
      };
  };

  aspects.graphical.home =
    { pkgs, ... }:
    {
      home.packages = [
        pkgs.colorpanes # <- print panes in the 8 bright terminal colors with shadows of the respective darker color
        pkgs.pastel # <- generate, analyze, convert and manipulate colors
      ];
    };
}
