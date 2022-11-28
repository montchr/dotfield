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
      bitwarden
      emacs
      graphical
      system-defaults
      virtualisation.containers
      yabai
    ]);
in {inherit workstation;}
