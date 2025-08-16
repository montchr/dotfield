flake@{ lib, ... }:
{
  dotfield.features.hardware__keyboardio.nixos =
    { config, ... }:
    let
      inherit (config.networking) hostName;
    in
    {
      imports = [ flake.config.nixosModules."hardware/keyboardio" ];

      hardware.keyboardio.enable = true;

      users.users = builtins.map (user: {
        ${user}.extraGroups = [ "plugdev" ];
      }) flake.config.dotfield.meta.hosts.${hostName}.admins;
    };
}
