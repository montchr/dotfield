{ self }:

{
  # Create a module from a given profile.
  # nixosModuleFromProfile = profile: { ... } @ args: (profile args);

  # Create a NixOS module that configures home-manager to use
  # the given profile.
  #
  # nixosConfigurationFromProfile = profile: username:
  #   { ... }@args: {
  #     home-manager.users.${username} = nixosModuleFromProfile profile;
  #   };

  # Create a homeConfiguration that can be installed with `home-manager --flake`.
  # homeConfigurationFromProfile = profile:
  #   { system, username ? "tlater", homeDirectory ? "/home/${username}" }:
  #   home-manager.lib.homeManagerConfiguration {
  #     inherit homeDirectory system username;
  #     configuration = nixosModuleFromProfile profile;
  #   };
}
