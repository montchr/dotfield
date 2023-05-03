{
  sharedProfiles,
  darwinProfiles,
}: let
  workstation =
    (with sharedProfiles; [
      fonts.common
      fonts.iosevka-variants
      # secrets
      # networking.tailscale
    ])
    ++ (with darwinProfiles; [
      one-password
      bitwarden
      emacs-plus
      graphical
      keyboard.atreus
      rclone
      system-defaults
      virtualisation.containers
    ]);
in {inherit workstation;}
