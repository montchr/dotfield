{
  sharedProfiles,
  darwinProfiles,
}: let
  workstation =
    [
      sharedProfiles.fonts.common
      sharedProfiles.fonts.iosevka-variants
      # sharedProfiles.secrets
      # sharedProfiles.networking.tailscale
    ]
    ++ [
      darwinProfiles.one-password
      darwinProfiles.bitwarden
      darwinProfiles.emacs-plus
      darwinProfiles.graphical
      darwinProfiles.keyboard.atreus
      darwinProfiles.rclone
      darwinProfiles.system-defaults
      darwinProfiles.virtualisation.containers
    ];
in {inherit workstation;}
