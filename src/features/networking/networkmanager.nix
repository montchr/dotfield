{ lib, ... }:
{
  dotfield.modules."networking/networkmanager".nixos =
    { config, ... }:
    lib.mkMerge [
      {
        networking.networkmanager.enable = true;
      }
    ]
    ++ (config.lib.generateSudoersExtraGroupsModules [ "networkmanager" ]);
}
