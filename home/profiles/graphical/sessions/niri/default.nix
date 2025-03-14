{
  imports = [
    ../../__wlroots.nix
  ];

  wayland.windowManager.niri = {
    enable = true;
    # Use package from NixOS.
    package = null;
  };

}
