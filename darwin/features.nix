{ sharedProfiles, darwinProfiles }:
let
  workstation = [
    sharedProfiles.fonts.common
    sharedProfiles.fonts.iosevka-variants
    sharedProfiles.secrets.default

    darwinProfiles.one-password
    darwinProfiles.bitwarden
    darwinProfiles.emacs.default
    # darwinProfiles.emacs.emacs-plus
    darwinProfiles.desktop.common
    darwinProfiles.keyboard.atreus
    darwinProfiles.rclone
    darwinProfiles.system-defaults
    darwinProfiles.virtualisation.containers
  ];
in
{
  inherit workstation;
}
