{config, lib, pkgs, ...}:
{
  environment.systemPackages = with pkgs; [acpi];
  services.upower.enable = true;
  services.logind.lidSwitchExternalPower = "ignore";
  services.logind.extraConfig = "HandlePowerKey=suspend";
}
