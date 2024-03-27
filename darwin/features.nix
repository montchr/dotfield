{ sharedProfiles, profiles }:
let
  workstation = [
    sharedProfiles.fonts.common
    sharedProfiles.fonts.iosevka-variants
    sharedProfiles.secrets.default

    profiles.one-password
    profiles.bitwarden
    profiles.emacs.default
    # profiles.emacs.emacs-plus
    profiles.desktop.common
    profiles.keyboard.atreus
    profiles.rclone
    profiles.system-defaults
    profiles.virtualisation.containers
  ];
in
{
  inherit workstation;
}
