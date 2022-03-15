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

  fd = "${pkgs.fd}/bin/fd";

  defaultCmd = "${fd} --hidden --follow --exclude .git 2>/dev/null";
in {
  home.sessionVariables = {
    # FIXME: this restricts the loaded color theme until changed by an *impure*
    # nixos/darwin rebuild, but a rebuild isn't quite fast enough to be usable
    # right now, and it requires sudo auth.
    #
    # perhaps the home-manager cli tool will allow for fast switching?
    #
    # FZF_DEFAULT_OPTS = ''"$FZF_DEFAULT_OPTS" --color="${mkTheme' currentTheme}"'';

    FZF_DEFAULT_COMMAND = defaultCmd;

    FZF_CTRL_T_COMMAND = defaultCmd;
    FZF_ALT_C_COMMAND = "${fd} --type d . $HOME";
  };
}
