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
        home.packages = [
          pkgs.chawan # <- tui web browser
          pkgs.fx
          pkgs.glow
          pkgs.hexyl
          pkgs.monolith # <- bundle any web page into a single html file   => <https://github.com/Y2Z/monolith>
          pkgs.moreutils
          pkgs.ouch
          pkgs.watchexec

          ##: color utils
          pkgs.colorpanes # <- print panes in the 8 bright terminal colors with shadows of the respective darker color
          pkgs.sanctity # <- ruSt ANsi16 Color Test utIliTY
          pkgs.pastel # <- generate, analyze, convert and manipulate colors
          (pkgs.writeShellApplication {
            name = "color-panic";
            runtimeInputs = [ pkgs.colorpanes ];
            text = ''
              colorpanes --captions --height 38 --width 24
            '';
          })

          ##: [TODO]
          pkgs.duf # <- better du/df alternative                          => <https://github.com/muesli/duf/>
        ];
      };
  };
}
