{
  imports = [
    ../profiles/graphical/common.nix
    ../profiles/hardware/power.nix
    ../profiles/hardware/yubikey.nix
    ../profiles/networking/common.nix
    ../profiles/theme.nix
  ];

  nix.settings.auto-optimise-store = true;
}
