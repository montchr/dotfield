{
  aspects.core = {
    nixos = {
      # XXX: yeah it's broken lazy-options.json non-deterministic fail
      # (can't revert to earlier flake.lock!!!)
      documentation.nixos.enable = false;

    };
  };
}
