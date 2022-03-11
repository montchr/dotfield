{ config, lib, pkgs, inputs, ... }:

let
  currentTheme = config.colorscheme.slug;

  fd = "${pkgs.fd}/bin/fd";

  mkTheme = name:
    with inputs.nix-colors.colorSchemes.${name}.colors; {
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

  mkTheme' = name: lib.concatStringsSep ","
    (lib.mapAttrsToList (n: v: "${n}:#${v}") (mkTheme name));

  defaultCmd = "${fd} --hidden --follow --exclude .git 2>/dev/null";
in
{
  my.hm.home.sessionVariables = {
    # FIXME: this restricts the loaded color theme until changed by a
    # nixos/darwin rebuild, which isn't really usable right now.
    # FZF_DEFAULT_OPTS = ''"$FZF_DEFAULT_OPTS" --color="${mkTheme' currentTheme}"'';

    FZF_DEFAULT_COMMAND = defaultCmd;

    FZF_CTRL_T_COMMAND = defaultCmd;
    FZF_ALT_C_COMMAND = "${fd} --type d . $HOME";
  };
}
