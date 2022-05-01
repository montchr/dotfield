{
  config,
  lib,
  pkgs,
  ...
}: let
  mkTheme = with config.colorscheme.colors;
    lib.concatStringsSep ","
    (lib.mapAttrsToList (n: v: "${n}:#${v}") {
      "bg" = base00;
      "bg+" = base01;
      "fg" = base04;
      "fg+" = base06;
      "header" = base0D;
      "hl" = base0D;
      "hl+" = base0D;
      "info" = base0A;
      "marker" = base0C;
      "pointer" = base0C;
      "prompt" = base0A;
      "spinner" = base0C;
    });

  fdBin = "${pkgs.fd}/bin/fd";
in {
  programs.fzf = {
    enable = true;
    defaultCommand = "${fdBin} --hidden --follow --exclude .git 2>/dev/null";
    changeDirWidgetCommand = "${fdBin} --type d . $HOME";
    fileWidgetCommand = "${fdBin} --type f --hidden";
  };
}
