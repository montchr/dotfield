{
  sharedProfiles,
  darwinProfiles,
}: let
  workstation = [
    sharedProfiles.fonts.common
    sharedProfiles.fonts.iosevka-variants

    darwinProfiles.one-password
    darwinProfiles.bitwarden
    darwinProfiles.desktop.common
    darwinProfiles.keyboard.atreus
    darwinProfiles.rclone
    darwinProfiles.system-defaults
    darwinProfiles.virtualisation.containers
  ];
in {inherit workstation;}
