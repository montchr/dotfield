{lib, ...}: {
  boot.consoleLogLevel = lib.mkDefault 3;
  boot.tmp.cleanOnBoot = true;

  # Enable all Magic SysRq functions via ALT-PrtSc-<command key>
  # https://www.kernel.org/doc/html/latest/admin-guide/sysrq.html
  # FIXME: GNOME overrides the default Magic SysRq binding to take a screenshot.
  boot.kernel.sysctl."kernel/sysrq" = 1;
}
