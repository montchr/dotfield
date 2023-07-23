{lib, ...}: {
  boot.loader.systemd-boot = {
    enable = true;
    consoleMode = lib.mkDefault "auto";
    configurationLimit = lib.mkDefault 10;
    editor = false;
  };
}
