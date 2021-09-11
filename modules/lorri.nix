{ pkgs, lib, config, inputs, ... }:
let cfg = config.my.modules.lorri;
in
{
  options = with lib; {
    my.modules.lorri = {
      enable = mkEnableOption ''
        Whether to enable lorri module
      '';
    };
  };

  config = with lib; mkIf cfg.enable { services.lorri.enable = true; };
}
