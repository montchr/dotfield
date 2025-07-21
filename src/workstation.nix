{ config, ... }:
{
  flake.modules.nixos.workstation = {
    imports = [
      config.flake.modules.nixos.graphical
    ];

    documentation.info.enable = true;
    # HACK: Force override <numtide/srvos>.
    documentation.man.enable = lib.mkForce true;

    services.power-profiles-daemon.enable = true;
    services.logind.extraConfig = ''
      HandlePowerKey=suspend
    '';

    nix.settings.auto-optimise-store = true;
  };

  flake.modules.homeManager.workstation = {
    dconf.settings."org/gnome/settings-daemon/plugins/power" = {
      power-button-action = "suspend";
      # FIXME: this isn't great, should suspend after a while
      sleep-inactive-ac-type = "nothing";
    };
  };

}
