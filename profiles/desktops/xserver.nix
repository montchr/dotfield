{
  config,
  lib,
  pkgs,
  ...
}: {
  environment.systemPackages = with pkgs; [
    libinput
    xclip
    xdg-utils
    xsel
  ];

  services.xserver = {
    enable = true;
    displayManager.lightdm.enable = lib.mkDefault true;
    layout = lib.mkDefault "us";
    xkbOptions = "ctrl:swapcaps";
    libinput.enable = true;
  };

  environment.sessionVariables = {
    XDG_CURRENT_DESKTOP = lib.mkDefault "X-Generic";
    DE = lib.mkDefault "generic";
  };

  console.useXkbConfig = lib.mkDefault true;
}
