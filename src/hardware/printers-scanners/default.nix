{
  flake.modules.nixos.workstation =
    { config, ... }:
    {
      services.printing.enable = true;

      # scanner support
      hardware.sane = {
        enable = true;
        openFirewall = config.hardware.sane.enable;
      };
    };
}
