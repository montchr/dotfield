{
  aspects.core.nixos = { config, lib, ... }: {
    nix.settings.auto-optimise-store = false;

    # De-duplicate store paths using hardlinks except in containers
    # where the store is host-managed.
    nix.optimise.automatic = lib.mkDefault (!config.boot.isContainer);

    nix.gc.dates = lib.mkDefault "weekly";
    nix.gc.automatic = lib.mkDefault true;

    systemd.services.nix-gc.serviceConfig = {
      CPUSchedulingPolicy = "batch";
      IOSchedulingClass = "idle";
      IOSchedulingPriority = 7;
    };
  };
}
