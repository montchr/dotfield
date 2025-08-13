flake@{ ... }:
{
  dotfield.nixos =
    { config, ... }:
    let
      inherit (config.networking) hostName;
    in
    {
      /**
        Generate a list of modules adding the specified groups to the
        host's administrators as specified in metadata.
      */
      lib.generateAdminExtraGroupsModules =
        groups:
        builtins.map (user: {
          users.users.${user}.extraGroups = groups;
        }) flake.dotfield.meta.hosts.${hostName}.admins;
    };
}
