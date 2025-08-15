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
    features = (
      with config.dotfield.features;
      [
        workstation
        gnome
        jobwork
        # remote-builders-default

        "hardware/razer"
      ]
    );

    imports = (
      with nixos-hardware.nixosModules;
      [
        common-cpu-amd
        common-cpu-amd-pstate
        common-gpu-amd
      ]
    );

    users.cdom = {
      features =
        cfg.features
        ++ (with config.dotfield.features; [
          gpg

          "git/with-gpg-signing"
          "gpg/with-ssh-support"
        ]);
      home.home.stateVersion = "22.05";
    };

    users.median = {
      features = (with config.dotfield.features; [ workstation ]);
      home.home.stateVersion = "24.05";
    };

    nixos = {
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
