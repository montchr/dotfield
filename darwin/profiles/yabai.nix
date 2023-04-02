{
  pkgs,
  self,
  inputs,
  ...
}: let
  inherit (self.lib.apps.yabai) mkSetting;

  l = inputs.nixpkgs.lib // builtins;

  defaultPadding = "6";
in {
  homebrew.taps = ["koekeishiya/formulae"];
  homebrew.brews = ["skhd" "yabai"];

  environment.systemPackages = [
    (pkgs.writeShellScriptBin "yabai-relaunch" ''brew services restart yabai'')
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
    (pkgs.writeShellScriptBin "yabai-focus-direction" ''
      #
      # Focus a window in the specified direction.
      #
      # Usage:
      #   yabai-focus-direction ( north | south | east | west ) [--displays]
      #
      # FIXME: When switching spaces, focus the closest window to the entry direction.
      #        E.G. `yabai -m space --focus next`, then: focus the western-most window.
      #        E.G. `yabai -m space --focus prev`, then: focus the eastern-most window.
      #

      DIRECTION=$1
      shift

      yabai -m window --focus "$DIRECTION" \
        && return 0

      for arg in "$@"; do
        case "$arg" in
          --displays)
            yabai -m display --focus "$DIRECTION" \
              && return 0
            ;;
          *) : ;;
        esac
      done

      case "$DIRECTION" in
        north)
          yabai -m window --focus stack.prev || yabai -m window --focus stack.last
          ;;
        south)
          yabai -m window --focus stack.next || yabai -m window --focus stack.first
          ;;
        east)
          yabai -m space --focus next
          ;;
        west)
          yabai -m space --focus prev
          ;;
      esac
    '')
  ];
}
