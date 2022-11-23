{
  lib,
  ...
}: {
  boot = {
    # TODO: consider setting this at some higher level... VMs may not want this...
    cleanTmpDir = lib.mkDefault true;
    consoleLogLevel = lib.mkDefault 3;

    # Enable all Magic SysRq functions via ALT-PrtSc-<command key>
    # https://www.kernel.org/doc/html/latest/admin-guide/sysrq.html
    # FIXME: GNOME overrides the default Magic SysRq binding to take a screenshot.
    kernel.sysctl."kernel/sysrq" = 1;
  };
}
