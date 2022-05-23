{ config, lib, pkgs, ... }:

{
  services.xserver.enable = true;
  services.xserver.libinput.enable = true;

  # Configure keymap in X11
  services.xserver.layout = "us";
  # TODO: might be flaky or need a kick?
  services.xserver.xkbOptions = "caps:ctrl_modifier";

  programs.mtr.enable = true;
  programs.gnupg.agent = {
    enable = true;
    enableSSHSupport = lib.mkDefault true;
  };

  # TODO: might only be available on master?
  # programs._1password-gui.enable = true;
  # programs._1password.enable = true;

  environment.variables = {
    # TODO: still necessary with firefox-wayland package?
    MOZ_ENABLE_WAYLAND = "1";
  };

  environment.systemPackages = with pkgs; [
    _1password
    _1password-gui
    firefox-wayland
  ];
}
