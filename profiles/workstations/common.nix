{ config, lib, pkgs, ... }:

{
  services.xserver.enable = true;
  services.xserver.libinput.enable = true;

  # Configure keymap in X11
  services.xserver.layout = "us";
  # TODO: might be flaky or need a kick?
  # FIXME: propagate to GNOME settings
  services.xserver.xkbOptions = "caps:ctrl_modifier";

  programs.mtr.enable = true;
  # TODO: does this conflict with the gpg hm-agent module?
  programs.gnupg.agent = {
    enable = true;
    enableSSHSupport = lib.mkDefault true;
  };

  # TODO: might only be available on master?
  # programs._1password-gui.enable = true;
  # programs._1password.enable = true;

  environment.systemPackages = with pkgs; [
    _1password
    _1password-gui
    # Avoid conflicts with our wrapped version from home-manager
    # TODO: define the wrapped version in an overlay
    # firefox-wayland
    wl-clipboard
  ];
}
