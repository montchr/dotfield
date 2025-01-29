{
  flake,
  pkgs,
  config,
  ...
}:
{
  imports = [
    ./desktop.nix

    ../profiles/audio.nix
    ../profiles/networking/avahi.nix

    ../profiles/graphical/applications/obs-studio.nix

    ../profiles/hardware/android-devices/default.nix
    ../profiles/hardware/bluetooth.nix
    ../profiles/hardware/bluetooth-headset.nix
    ../profiles/hardware/keyboard/default.nix
    ../profiles/hardware/printers-scanners/default.nix

    ../profiles/protonmail-bridge.nix
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

  # NOTE: This will significantly slow down builds.  However, it enables more
  # manpage integrations across various tools (e.g. `apropos`, `man -k`).
  documentation.man.generateCaches = true;
}
