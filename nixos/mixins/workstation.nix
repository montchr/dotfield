{ pkgs, config, ... }:
{
  imports = [
    ../profiles/graphical/common.nix

    ../profiles/audio.nix
    ../profiles/networking/avahi.nix

    ../profiles/hardware/android-devices/default.nix
    ../profiles/hardware/bluetooth.nix
    ../profiles/hardware/bluetooth-headset.nix
    ../profiles/hardware/keyboard/default.nix
    ../profiles/hardware/power.nix
    ../profiles/hardware/printers-scanners/default.nix
    ../profiles/hardware/yubikey.nix
  ];

  programs._1password.enable = true;
  programs._1password-gui = {
    enable = true;
    polkitPolicyOwners = config.users.groups.wheel.members;
  };

  location.provider = "geoclue2";
  services.geoclue2.enable = true;

  services.dictd.enable = true;
  services.dictd.DBs = with pkgs.dictdDBs; [
    deu2eng
    eng2deu
    wiktionary
    wordnet
  ];

  programs.nh = {
    enable = true;
    # <https://github.com/viperML/nh/issues/88>
    flake = "/etc/nixos";
  };

}
