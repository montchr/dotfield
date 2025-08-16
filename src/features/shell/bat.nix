{
  pkgs,
  ...
}:
{
  dotfield.baseline.home = {
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

    home.packages = with pkgs; [
      # Bash scripts that integrate bat with various command line tools.
      # https://github.com/eth-p/bat-extras/
      bat-extras.batman # <- Read system manual pages (man) using bat as the manual page formatter.
      bat-extras.batgrep # <- Quickly search through and highlight files using ripgrep.
    ];
  };
}
