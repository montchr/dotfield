{lib, ...}: {
  boot.loader = {
    grub.enable = false;
    systemd-boot = {
      enable = true;
      consoleMode = lib.mkDefault "auto";
      # This number is super low -- it can safely be bumped higher, but be
      # careful otherwise you may fill up a boot partition pretty quickly in
      # relation to increased frequency of rebuilds/generations.
      configurationLimit = lib.mkDefault 24;
      editor = false;
    };
  };
}
