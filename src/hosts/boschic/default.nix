{ config, ... }:
let
  inherit (config.dotfield.meta) keys;
  inherit (config.dotfield) aspects;
  hostName = "boschic";
in
{
  dotfield.hosts.nixos.boschic = {
    aspects = [
      aspects.workstation
      aspects.gnome-desktop
      aspects.hardware__amd__cpu
      aspects.hardware__focusrite__scarlett-18i20-mk1
      aspects.hardware__nvidia
      aspects.hardware__razer
    ];

    nixos = {
      services.tailscale.enable = true;

      programs.steam.enable = true;
      services.flatpak.enable = true;

      virtualisation.vmVariant = {
        virtualisation.graphics = false;
      };

      users.mutableUsers = false;
      users.groups.wheel.members = [ "seadoom" ];
      sops.defaultSopsFile = ./secrets/secrets.yaml;

      system.stateVersion = "21.11";
    };
  };

  dotfield.meta.hosts.boschic = {
    admins = [ "seadoom" ];
    ipv4.address = "192.168.1.214";
    keys = {
      age = keys.age.${hostName};
      ssh = [
        keys.ssh.${hostName}
        keys.ssh."${hostName}-rsa"
      ];
    };
    network = "home";
    networks.ts = "100.112.94.38";
    users.seadoom.keys = {
      age = keys.age.seadoom-at-boschic;
      ssh = [ keys.ssh.seadoom-at-boschic ];
    };
    syncthing.id = "5TCUNJM-PVGGNJ6-DETAT3O-PSMTOEP-SXRT7FP-62EFNZY-6ENFIYZ-3J2VHQJ";
  };
}
