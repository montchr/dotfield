{
  aspects.core.nixos = { config, lib, ... }: {
    nix.settings.auto-optimise-store = false;

    # De-duplicate store paths using hardlinks except in containers
    # where the store is host-managed.
    nix.optimise.automatic = lib.mkDefault (!config.boot.isContainer);

    nix.gc.dates = lib.mkDefault "weekly";
    nix.gc.automatic = lib.mkDefault (!config.programs.nh.clean.enable);

    # Without this the collector only reaps paths with zero roots, which
    # every retained NixOS generation prevents, thus resulting in
    # indefinitely increasing disk usage.
    nix.gc.options = lib.mkDefault "--delete-older-than 14d";

    systemd.services.nix-gc.serviceConfig = {
      CPUSchedulingPolicy = "batch";
      IOSchedulingClass = "idle";
      IOSchedulingPriority = 7;
    };
  };
}
