{
  sharedProfiles,
  darwinProfiles,
}: let
  workstation =
    (with sharedProfiles; [
      one-password
      fonts.common
      fonts.iosevka-variants
      # secrets
      # networking.tailscale
    ])
    ++ (with darwinProfiles; [
      emacs
      graphical.common
      # graphical.hammerspoon
      graphical.yabai
      system-defaults
      virtualisation.containers
      # graphical.sketchybar
    ]);
in {inherit workstation;}
