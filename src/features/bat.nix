{
  aspects.core = {
    home =
      { pkgs, ... }:
      {
        home.shellAliases."grr" = "${pkgs.bat-extras.batgrep}/bin/batgrep";
        home.shellAliases."man" = "${pkgs.bat-extras.batman}/bin/batman";

        home.packages = with pkgs; [
          bat-extras.batman # <- Read system manual pages (man) using bat as the manual page formatter.
          bat-extras.batgrep # <- Quickly search through and highlight files using ripgrep.
        ];

        programs.bat = {
          enable = true;
          config = {
            map-syntax = [
              ".*ignore:Git Ignore"
              ".gitconfig.local:Git Config"
              "**/mx*:Bourne Again Shell (bash)"
              "**/completions/_*:Bourne Again Shell (bash)"
              ".vimrc.local:VimL"
              "vimrc:VimL"
            ];
          };
        };
      };
  };
}
