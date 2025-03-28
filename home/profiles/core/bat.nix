{ lib, pkgs, ... }:
{
  home.shellAliases."grr" = "${pkgs.bat-extras.batgrep}/bin/batgrep";
  home.shellAliases."man" = "${pkgs.bat-extras.batman}/bin/batman";

  home.packages = with pkgs; [
    # Bash scripts that integrate bat with various command line tools.
    # https://github.com/eth-p/bat-extras/
    bat-extras.batman # <- Read system manual pages (man) using bat as the manual page formatter.
    bat-extras.batgrep # <- Quickly search through and highlight files using ripgrep.
    bat-extras.batwatch # <- Watch for changes in files or command output, and print them with bat.
    bat-extras.prettybat # <- Pretty-print source code and highlight it with bat.
    # XXX: broken with rust v1.80 <https://github.com/NixOS/nixpkgs/issues/332957>
    # bat-extras.batdiff # <- Diff a file against the current git index, or display the diff between two files.
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
}
