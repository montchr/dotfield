{
  sharedProfiles,
  darwinProfiles,
}: let
  workstation =
    [
      sharedProfiles.one-password
      (sharedProfiles."fonts/common")
      (sharedProfiles."fonts/iosevka-variants")
      # secrets
      # networking.tailscale
    ]
    ++ (with darwinProfiles; [
      bitwarden
      emacs-plus
      graphical
      keyboard.atreus
      system-defaults
      virtualisation.containers
    ]);
in {inherit workstation;}
