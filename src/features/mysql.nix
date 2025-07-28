{ lib, ... }:
{
  dotfield.modules.mysql.nixos =
    { config, ... }: lib.mkMerge [ ] ++ (config.lib.generateSudoersExtraGroupsModules [ "mysql" ]);
}
