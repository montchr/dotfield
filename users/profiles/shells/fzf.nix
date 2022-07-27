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
    defaultOptions = [
      "--height 40%"
      "--border"
      "--no-multi"
    ];
    changeDirWidgetCommand = "${fdBin} --type d .";
    fileWidgetCommand = "${fdBin} --type f --hidden --max-depth 5";
  };

  home.packages = with pkgs; [
    # Use ripgrep interactively
    # https://github.com/DanielFGray/fzf-scripts/blob/7cf2925b0194f0ad116b84e8f45d8f01a87c774f/igr
    (writeShellApplication {
      name = "igr";
      runtimeInputs = [bat fzf ripgrep];
      text = ''
        ${sources.fzf-scripts.src}/igr "$@"
      '';
    })
  ];
}
