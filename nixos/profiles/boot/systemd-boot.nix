{ lib, ... }:
{
  boot.loader.systemd-boot = {
    enable = true;
    consoleMode = lib.mkDefault "auto";
    configurationLimit = lib.mkDefault 10;
    # NixOS manual recommends setting this to false, as it allows gaining root
    # access by passing `init=/bin/sh` as a kernel parameter. It's enabled by
    # default for back-compat.
    editor = false;
  };
  # boot.loader.timeout = lib.mkDefault 3;
}
