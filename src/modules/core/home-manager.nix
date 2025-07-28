{ inputs, ... }:
{
  dotfield.modules.hosts.nixos =
    { config, ... }:
    {
      imports = [ inputs.home-manager.nixosModules.home-manager ];
      home-manager = {
        settings = {
          useGlobalPkgs = true;
          useUserPackages = true;
        };
      };
    };

  dotfield.home.defaults =
    { ... }:
    {
      programs.home-manager.enable = true;
    };
}
