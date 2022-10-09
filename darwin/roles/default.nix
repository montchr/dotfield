{
  sharedProfiles,
  darwinProfiles,
}: let
  workstation =
    (with sharedProfiles; [
      one-password
      fonts.common
      networking.common
      # secrets
      # networking.tailscale
    ])
    ++ (with darwinProfiles; [
      emacs
      graphical.common
      # graphical.hammerspoon
      # graphical.yabai
      system-defaults
      # graphical.sketchybar
    ]);
in {inherit workstation;}
