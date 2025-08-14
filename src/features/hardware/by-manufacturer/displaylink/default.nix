{
  dotfield.features."hardware/displaylink".nixos = {
    # For details on update procedure, follow the messages shown during the
    # initial rebuild, and/or see <https://wiki.nixos.org/wiki/Displaylink>.
    services.xserver.videoDrivers = [
      "displaylink"
      "modesetting"
    ];
  };
}
