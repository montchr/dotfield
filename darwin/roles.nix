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
      emacs-plus
      graphical
      keyboard.atreus
      system-defaults
      virtualisation.containers
    ]);
in {inherit workstation;}
