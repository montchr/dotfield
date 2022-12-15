{
  config,
  pkgs,
  inputs,
  self,
  ...
}: let
  # inherit (self.lib.apps.fzf) makeThemeAttrs;
  # inherit (config) theme;
  l = inputs.nixpkgs.lib // builtins;

  exa = l.getExe pkgs.exa;
  fd = l.getExe pkgs.fd;

  findFiles = "${fd} --hidden --follow --exclude '.git'";
  findDirs = "${fd} --type d";
in {
  programs.fzf = {
    enable = true;
    # colors = l.mkIf theme.enable (makeThemeAttrs theme.colors.active);

    defaultCommand = "${findFiles} 2>/dev/null";
    defaultOptions = [
      "--ansi"
      "--reverse"
    ];
    changeDirWidgetCommand = findDirs;
    changeDirWidgetOptions = ["--preview '${exa} --tree {} | head -n 200'"];
    fileWidgetCommand = findFiles;
    fileWidgetOptions = ["--preview 'head {}'"];
    historyWidgetOptions = ["--sort" "--exact"];
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
