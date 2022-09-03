{
  sharedProfiles,
  darwinProfiles,
}: let
  workstation =
    (with sharedProfiles; [
      fonts.common
      networking.common
      networking.ssh-host
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
