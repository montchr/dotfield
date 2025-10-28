{
  hosts.nixos.riebeck.baseline.home = {
    services.kanshi.enable = true;
    services.kanshi.settings = [
      {
        output.criteria = "eDP-1";
        output.scale = 2.0;
      }
    ];
  };
}
