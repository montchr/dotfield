{ lib, ... }:
{
  dotfield.features."networking/networkmanager".nixos =
    { config, ... }:
    lib.mkMerge [
      {
        networking.networkmanager.enable = true;
      }
    ]
    ++ (config.lib.generateSudoersExtraGroupsModules [ "networkmanager" ]);
}
