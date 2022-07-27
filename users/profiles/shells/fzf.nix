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
    (writeShellApplication {
      name = "igr";
      runtimeInputs = [bat fzf ripgrep];
      text = ''
        # Copyright 2020, Daniel F. Gray and the fzf-scripts contributors
        # SPDX-License-Identifier: GPL-3.0-only
        # https://github.com/DanielFGray/fzf-scripts/blob/7cf2925b0194f0ad116b84e8f45d8f01a87c774f/igr
        declare preview='bat --color=always --style=header,numbers -H {2} {1} | grep -C3 {q}'

        while getopts ':l' x; do
          case "$x" in
            l) list_files=1
              preview='bat --color=always --style=header,numbers {1} | grep -C3 {q}'
              ;;
            *) continue ;;
          esac
        done
        shift $(( OPTIND - 1 ))
        unset x OPTARG OPTIND

        rg --color=always -n ''${list_files:+-l} "$1" 2> /dev/null |
        fzf -d: \
        --ansi \
        --query="$1" \
        --phony \
        --bind="change:reload:rg -n ''${list_files:+-l} --color=always {q}" \
        --bind='enter:execute:v {1}' \
        --preview="[[ -n {1} ]] && $preview"
      '';
    })
  ];
}
