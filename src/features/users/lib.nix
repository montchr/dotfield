{
  dotfield.nixos =
    { config, ... }:
    {
      /**
        Generate a list of modules adding the specified groups to existing
          members of the `wheel` group.
      */
      lib.generateSudoersExtraGroupsModules =
        groups:
        builtins.map (username: {
          users.users.${username}.extraGroups = groups;
        }) config.users.groups.wheel.members;
    };
}
