{
  imports = [
    ../profiles/graphical/common.nix
    ../profiles/hardware/power.nix
    ../profiles/hardware/yubikey.nix
    ../profiles/networking/common.nix
  ];

  nix.settings.auto-optimise-store = true;
}
