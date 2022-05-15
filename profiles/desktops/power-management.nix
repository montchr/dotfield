{
  config,
  lib,
  pkgs,
  ...
}: {
  environment.systemPackages = with pkgs; [acpi];
  services.acpid.enable = true;
  services.upower.enable = true;
  services.logind.lidSwitchExternalPower = "ignore";
  services.logind.extraConfig = "HandlePowerKey=suspend";
}
