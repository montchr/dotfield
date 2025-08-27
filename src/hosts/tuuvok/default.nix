flake@{
  lib,
  inputs,
  moduleWithSystem,
  ...
}:
let
  inherit (flake.config.dotfield.meta) hosts keys;
in
{
  dotfield.hosts.nixos.tuuvok = {
    channel = "nixpkgs-apple-silicon";
    aspects = with flake.config.dotfield.aspects; [
      sway
      workstation

      greeters__regreet
      hardware__apple__macbook-14-2

      # DisplayLink is currently required for Asahi monitor support.
      # Asahi does not yet support standard DP-Alt display output.
      # DP-Alt output is required for true HDMI or DP output via one
      # of this machine's two USB-C ports and zero HDMI/DP ports.
      # DisplayLink provides a proprietary video-over-USB protocol
      # requiring special external hardware, either a dock or a
      # monitor natively supporting DisplayLink, but for now, I'll
      # take it.
      hardware__displaylink
    ];

    nixos = moduleWithSystem (
      perSystem@{ inputs', config, ... }:
      nixos@{ config, ... }:
      {
        imports = [
          inputs.nixos-apple-silicon.nixosModules.apple-silicon-support
          inputs.beams.modules.nixos.default
        ];

        nixpkgs.overlays = lib.mkBefore [
          inputs.nixos-apple-silicon.overlays.default
        ];

        # NOTE: The firmware "asahi-tuuvok-firmware" repository results in
        # broken wifi.  Reverting to the "asahi-tuvok-firmware" repository works.
        hardware.asahi.peripheralFirmwareDirectory =
          perSystem.inputs'.asahi-tuvok-firmware.packages.default;

        services.tailscale.enable = true;

        fonts.packages = [ perSystem.config.packages.berkeley-mono ];

        users.mutableUsers = false;
        sops.defaultSopsFile = ./secrets/secrets.yaml;
        services.displayManager.autoLogin.enable = true;
        services.displayManager.autoLogin.user = "cdom";

        # Not allowed because I don't want to make the building's network
        # switch mad again.
        # TODO: Should be disabled by default?
        services.avahi.enable = lib.mkForce false;

        system.stateVersion = "23.11"; # Did you read the comment?
      }
    );

    baseline.home = {
      imports = [
        inputs.beams.modules.homeManager.default
      ];
    };
  };

  dotfield.meta.hosts.tuuvok = {
    hardware = {
      # We share a body...
      inherit (hosts.tuvix.hardware) mem vcpus;
      # ...different minds, same brain.
      system = "aarch64-linux";
    };
    keys = {
      age = keys.age.tuuvok;
      ssh = [
        keys.ssh.tuuvok
        keys.ssh.tuuvok-rsa
      ];
    };
    networks.ts = {
      ipv4 = "100.89.80.26";
      ipv6 = "fd7a:115c:a1e0::1c01:501a";
    };
    users.cdom.keys = {
      age = keys.age.cdom-at-tuuvok;
      ssh = [ keys.ssh.cdom-at-tuuvok ];
    };
    syncthing.id = "TR3RHZG-CZX3C6D-N2SDPVS-RI2H4JR-DEAVMKT-O7V4US2-LQK5WNR-V2TN2AA";
  };
}
