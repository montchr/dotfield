# <https://github.com/nix-community/srvos/blob/c4bc7f50aaa6b2e120603790d7ffc509c4584ab7/nixos/common/nix.nix#L14-L17>
# <https://discourse.nixos.org/t/nix-build-ate-my-ram/35752>
{
  aspects.core.nixos = { lib, ... }: {
    nix.daemonCPUSchedPolicy = lib.mkDefault "batch";
    nix.daemonIOSchedClass = lib.mkDefault "idle";
    nix.daemonIOSchedPriority = lib.mkDefault 7;

    # Make builds to be more likely killed than important services.
    #
    # 100 is the default for user slices and 500 is `systemd-coredumpd@`.
    # We rather want a build to be killed than our precious user sessions
    # as builds can be easily restarted.
    systemd.services.nix-daemon.serviceConfig.OOMScoreAdjust = lib.mkDefault 250;
  };
}
