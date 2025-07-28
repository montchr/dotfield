{
  dotfield.modules."boot/systemd-boot".nixos = {
    boot.loader.systemd-boot = {
      enable = true;
      consoleMode = "auto";
      configurationLimit = 16;
      # NixOS manual recommends setting this to false, as it allows gaining root
      # access by passing `init=/bin/sh` as a kernel parameter. It's enabled by
      # default for back-compat.
      editor = false;
    };
  };
}
