{ lib, ... }:
let
  workspaces = [
    "0"
    "1"
    "2"
    "3"
    "4"
    "5"
    "6"
    "7"
    "8"
    "9"
  ];

  # Map keys (arrows and hjkl) to hyprland directions (l, r, u, d)
  dirs = rec {
    left = "l";
    right = "r";
    up = "u";
    down = "d";
    h = left;
    l = right;
    k = up;
    j = down;
  };
in
{
  wayland.windowManager.hyprland.settings = {
    bindm = [
      "$mod, mouse:272, movewindow"
      "$mod, mouse:273, resizewindow"
    ];
    bind = [
      "$mod SHIFT, q, killactive"
      # NOTE: Do not use `exit` in combination with UWSM!  This will
      # yank compositor from under all the clients, interfere with
      # ordered unit deactivation sequence, and leave you in a black
      # screen prison.
      # <https://github.com/Vladimir-csp/uwsm?tab=readme-ov-file#how-to-stop>
      "$mod SHIFT, e, exec, uwsm stop"

      "$mod, s, togglesplit"
      "$mod, f, fullscreen, 1"
      "$mod SHIFT, f, fullscreen, 0"
      "$mod SHIFT, space, togglefloating"

      "$mod, minus, splitratio, -0.25"
      "$mod SHIFT, minus, splitratio, -0.3333333"

      "$mod, equal, splitratio, 0.25"
      "$mod SHIFT, equal, splitratio, 0.3333333"

      "$mod, g, togglegroup"
      "$mod, t, lockactivegroup, toggle"
      "$mod, tab, changegroupactive, f"
      "$mod SHIFT, tab, changegroupactive, b"

      "$mod, apostrophe, workspace, previous"
      "$mod SHIFT, apostrophe, workspace, next"
      "$mod, dead_grave, workspace, previous"
      "$mod SHIFT, dead_grave, workspace, next"

      "$mod, u, togglespecialworkspace"
      "$mod SHIFT, u, movetoworkspacesilent, special"
      # "$mod, i, pseudo"
    ]
    ++
      # Change workspace
      (map (n: "$mod, ${n}, workspace, name:${n}") workspaces)
    ++
      # Move window to workspace
      (map (n: "$mod SHIFT, ${n}, movetoworkspacesilent, name:${n}") workspaces)
    ++
      # Move focus
      (lib.mapAttrsToList (key: dir: "$mod, ${key}, movefocus, ${dir}") dirs)
    ++
      # Swap windows
      (lib.mapAttrsToList (key: dir: "$mod SHIFT, ${key}, swapwindow, ${dir}") dirs)
    ++
      # Move windows
      (lib.mapAttrsToList (key: dir: "$mod CONTROL, ${key}, movewindow, ${dir}") dirs)
    ++
      # Move monitor focus
      (lib.mapAttrsToList (key: direction: "$mod ALT, ${key}, focusmonitor, ${direction}") dirs)
    ++
      # Move workspace to other monitor
      (lib.mapAttrsToList (
        key: direction: "$mod ALT SHIFT, ${key}, movecurrentworkspacetomonitor, ${direction}"
      ) dirs);
  };
}
