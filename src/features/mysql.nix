{ lib, ... }:
{
  dotfield.features.mysql.nixos =
    { config, ... }: lib.mkMerge [ ] ++ (config.lib.generateSudoersExtraGroupsModules [ "mysql" ]);
}
