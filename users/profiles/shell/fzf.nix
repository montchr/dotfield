{ config, lib, pkgs, ... }:

let
  inherit (config.colorscheme) colors;

  fd = "${pkgs.fd}/bin/fd";

  colorMap = with colors; {
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
  };

  colorOpts = lib.concatStringsSep ","
    (lib.mapAttrsToList (n: v: "${n}:#${v}") colorMap);

  defaultCmd = "${fd} --type f --hidden --follow --exclude .git 2>/dev/null";
in
{
  my.env = {
    FZF_DEFAULT_OPTS = ''"$FZF_DEFAULT_OPTS" --color="${colorOpts}"'';
    FZF_DEFAULT_COMMAND = defaultCmd;

    FZF_CTRL_T_COMMAND = defaultCmd;
    FZF_ALT_C_COMMAND = "${fd} --type d . $HOME";
  };
}
