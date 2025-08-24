{
  config,
  inputs,
  ...
}:
let
  inherit (inputs) nixos-hardware;
  inherit (config.dotfield.meta) keys;

  cfg = config.dotfield.hosts.ryosuke;
in
{
  dotfield.hosts.nixos.ryosuke = {
    aspects = (
      with config.dotfield.aspects;
      [
        workstation
        gnome-desktop
        # remote-builders-default
        hardware__razer
      ]
    );

    nixos = {
      imports = [
        inputs.beams.modules.nixos.default
      ]
      ++ (with nixos-hardware.nixosModules; [
        common-cpu-amd
        common-cpu-amd-pstate
        common-gpu-amd
      ]);

      networking.firewall.enable = true;
      services.tailscale.enable = true;

      programs.steam.enable = true;

      services.displayManager.autoLogin.enable = true;
      services.displayManager.autoLogin.user = "cdom";

      sops.defaultSopsFile = ./secrets/secrets.yaml;
      # Never remove old secrets (attempt to fix lockouts).
      sops.keepGenerations = 0;

      system.stateVersion = "22.05";
    };

    baseline.home = {
      imports = [
        inputs.beams.modules.homeManager.default
      ];
    };
  };

  dotfield.meta.hosts.ryosuke = {
    ipv4.address = "192.168.1.217";
    hardware = {
      mem = 32;
      vcpus = 24;
      system = "x86_64-linux";
    };
    keys.age = keys."ryosuke.age";
    keys.ssh = [
      keys.ryosuke
      keys.ryosuke-rsa
    ];
    network = "home";
    networks.ts.ipv4.address = "100.123.41.68";
    users.cdom.keys = {
      age = keys.age.cdom-at-ryosuke;
      ssh = [ keys.ssh.cdom-at-ryosuke ];
    };
    syncthing.id = "2HDN7UF-5YKEBC7-4YB4L4H-A6Y7EGS-YZ5CSQX-AWWDKR7-KH5WIKH-D6LOTQ4";
  };

}
