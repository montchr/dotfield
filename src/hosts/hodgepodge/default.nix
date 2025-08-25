{ lib, config, ... }:
let
  inherit (config.dotfield.meta) keys;
in
{
  dotfield.hosts.nixos.hodgepodge = {
    aspects = with config.dotfield.aspects; [
      hardware__apple__macbook-pro-11-3
      workstation
      gnome-desktop
      greeters__gdm
    ];

    nixos = {
      services.tailscale.enable = true;

      boot.loader.efi.canTouchEfiVariables = true;

      ## Hardware oddities specific to this machine:
      home-manager.sharedModules = lib.singleton {
        dconf.settings."org/gnome/desktop/peripherals/touchpad" = {
          # The trackpad button is physically broken.
          # Without tap-to-click, it would not possible to use the trackpad.
          tap-to-click = lib.mkForce true;
        };

        # The keyboard is also starting to go... but a super-thorough
        # deep clean might help.  Fortunately, no butterflies here.
        # Real and actual removable keycaps!  What a concept.
        dconf.settings."org/gnome/desktop/a11y/applications" = {
          screen-keyboard-enabled = lib.mkDefault true;
        };
      };

      networking.usePredictableInterfaceNames = false;
      networking.firewall.enable = true;

      users.mutableUsers = false;
      users.groups.wheel.members = [ "seadoom" ];
      sops.defaultSopsFile = ./secrets/secrets.yaml;

      services.displayManager.autoLogin.enable = true;
      services.displayManager.autoLogin.user = "seadoom";

      system.stateVersion = "21.11"; # Did you read the comment?
    };

  };

  dotfield.meta.hosts.hodgepodge = {
    admins = [ "seadoom" ];
    ipv4.address = "192.168.1.152";
    keys = {
      age = keys.age."hodgepodge";
      ssh = [ keys.hodgepodge ];
    };
    network = "home";
    networks.ts = "100.71.240.35";
    users.seadoom.keys.ssh = [ keys.ssh.seadoom-at-hodgepodge ];
    syncthing.id = "W7EFFEO-BAZIKPC-M5C2OOT-JXR6CIP-MISL4ID-2ZUBFYT-44ZEWUK-6R75OA3";
  };
}
