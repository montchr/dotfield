# Copyright 2020, Daniel F. Gray and the fzf-scripts contributors
# SPDX-License-Identifier: GPL-3.0-only
# https://github.com/DanielFGray/fzf-scripts/blob/7cf2925b0194f0ad116b84e8f45d8f01a87c774f/igr
{
  writeShellApplication,
  bat,
  fzf,
  ripgrep,
}:
(writeShellApplication {
  name = "igr";
  runtimeInputs = [
    bat
    fzf
    ripgrep
  ];
  text = ''
    declare query="$1"
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

    rg --color=always -n ''${list_files:+-l} "$query" 2> /dev/null \
    | fzf -d: \
      --ansi \
      --query="$query" \
      --phony \
      --bind="change:reload:rg -n ''${list_files:+-l} --color=always {q}" \
      --bind='enter:execute:v {1}' \
      --preview="[[ -n {1} ]] && $preview"
  '';
})
