{
  sharedProfiles,
  darwinProfiles,
}: let
  workstation =
    (with sharedProfiles; [
      one-password
      fonts.common
      networking.common
      networking.tailscale
      secrets
    ])
    ++ (with darwinProfiles; [
      emacs
      graphical.common
      # graphical.hammerspoon
      # graphical.sketchybar
      graphical.yabai
      system-defaults
    ]);
in {inherit workstation;}
