{ self, ... }:
{
  aspects.kanata.home = {
    imports = [ self.modules.homeManager.kanata ];

    services.kanata.enable = true;
  };
}
