{
  pkgs,
  flake,
  ...
}: let
  inherit (flake.inputs.apparat.lib.yabai) mkSetting;
  l = flake.inputs.nixpkgs.lib // builtins;

  defaultPadding = "6";
in {
  homebrew.taps = ["koekeishiya/formulae"];
  homebrew.brews = ["skhd" "yabai"];

  environment.systemPackages = [
    (pkgs.writeShellScriptBin "yabai-set-padding" ''
      PADDING="''${1:-''${YABAI_PADDING_DEFAULT:-${defaultPadding}}}"
      ${l.concatMapStrings (v: mkSetting v "$PADDING") [
        "top_padding"
        "bottom_padding"
        "left_padding"
        "right_padding"
        "window_gap"
      ]}
    '')

    (pkgs.writeShellScriptBin "yabai-window-focus" ''
      #!/usr/bin/env bash

      # yabai-window-focus
      #
      # Source: <https://github.com/d12frosted/environment/blob/48774588094fd2b1a10cf721eaaf5697f5f7a3d7/bin/yabai-window-focus>
      #
      # Focus a window in the specified direction.
      #
      # Usage:
      #   yabai-window-focus ( north | south | east | west )
      #

      direction="$1"

      case $direction in
        prev)
          yabai -m query --spaces \
            | jq -re '.[] | select(."is-visible").index' \
            | xargs -I{} yabai -m query --windows --space {} \
            | jq -sre 'add | map(select(."is-hidden" == false) | select(."is-minimized" == false)) | sort_by(.display, .frame.x, .frame.y, ."stack-index", .id) | reverse | nth(index(map(select(."has-focus"))) - 1).id' \
            | xargs -I{} yabai -m window --focus {}
          ;;
        next)
          yabai -m query --spaces \
            | jq -re '.[] | select(."is-visible").index' \
            | xargs -I{} yabai -m query --windows --space {} \
            | jq -sre 'add | map(select(."is-hidden" == false) | select(."is-minimized" == false)) | sort_by(.display, .frame.x, .frame.y, ."stack-index", .id) | nth(index(map(select(."has-focus"))) - 1).id' \
            | xargs -I{} yabai -m window --focus {}
          ;;
        north) yabai -m window --focus "$direction" ;;
        south) yabai -m window --focus "$direction" ;;
        *)
          echo "unknown direction '$direction'"
          exit 1
          ;;
      esac
    '')

    (pkgs.writeShellScriptBin "yabai-focus-direction" ''
      #
      # Focus a window in the specified direction.
      #
      # Usage:
      #   yabai-focus-direction ( north | south | east | west | next | prev )
      #
      # TODO: When switching spaces, focus the closest window to the entry direction.
      #        E.G. `yabai -m space --focus next`, then: focus the western-most window.
      #        E.G. `yabai -m space --focus prev`, then: focus the eastern-most window.
      #

      DIRECTION=$1

      yabai -m window --focus "$DIRECTION" \
        || yabai -m space --focus "$DIRECTION" \
        || yabai -m display --focus "$DIRECTION"
    '')
  ];
}
