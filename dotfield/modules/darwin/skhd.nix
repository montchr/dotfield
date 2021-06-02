{ config, lib, options, ... }:

{
  options = with lib; {
    my.modules.skhd = {
      enable = mkEnableOption ''
        Whether to enable skhd module
      '';
    };
  };

  config = {
    services.skhd = { enable = true; };
  };
}
