flake@{ lib, ... }:
let
  inherit (flake.config.dotfield) meta;

  hostAspects = flake.config.dotfield.hosts.nixos.tuuvok.aspects;
in
{
  dotfield.hosts.nixos.ash-twin = {
    aspects = with flake.config.dotfield.aspects; [
      graphical
      sway
    ];

    nixos =
      { modulesPath, ... }:
      {
        services.openssh.settings.PermitRootLogin = lib.mkForce "yes";

        users.users.cdom = {
          uid = 1000;
          isNormalUser = true;
          password = "cdom";
          openssh.authorizedKeys.keys = meta.users.cdom.keys.ssh;
        };

        # FIXME: handle this in a way that allows for building an iso
        # for other systems
        nixpkgs.hostPlatform = "aarch64-linux";

        system.stateVersion = "25.11";
      };

    users.cdom = {
      aspects = hostAspects;

      home = {
        home.stateVersion = "25.11";
      };
    };
  };
}
