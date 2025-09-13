{ lib, ... }:
{
  aspects.core = {
    nixos = {
      boot.consoleLogLevel = lib.mkDefault 3;
      boot.tmp.cleanOnBoot = true;

      boot.initrd.systemd.enable = true;

      # Enable all Magic SysRq functions via ALT-PrtSc-<command key>
      # https://www.kernel.org/doc/html/latest/admin-guide/sysrq.html
      # FIXME: GNOME overrides the default Magic SysRq binding to take a screenshot.
      boot.kernel.sysctl."kernel/sysrq" = 1;

      boot.loader.systemd-boot = {
        enable = true;
        consoleMode = "auto";
        configurationLimit = lib.mkDefault 16;
        # NixOS manual recommends setting this to false, as it allows gaining root
        # access by passing `init=/bin/sh` as a kernel parameter. It's enabled by
        # default for back-compat.
        editor = false;
      };
    };
  };
}
