{ lib, ... }:
{
  dotfield.modules.workstation.nixos =
    { config, ... }:
    lib.mkMerge [
      {
        services.printing.enable = true;
        hardware.sane = {
          enable = true;
          openFirewall = true;
        };
      }
    ]
    ++ (config.lib.generateSudoersExtraGroupsModules [
      "cups"
      "lp"
      "scanner"
    ]);
}
