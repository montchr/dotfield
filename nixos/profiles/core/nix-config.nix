{
  nix = {
    # TODO: always appropriate??
    settings.system-features = [
      "nixos-test"
      "benchmark"
      "big-parallel"
      "kvm"
    ];
    gc.dates = "weekly";
    optimise.automatic = true;
  };
}
