{
  builds = {
    default = {
      # What builds to include. This is a list of *attribute matchers*, of the
      # form `x.y.z` or `x.y`. For example, `packages.x86_64-linux.*`, or `*.*`.
      # Two-place matchers only match two-place matchers, and three-place
      # matchers only match three-place matchers. '*' is the wildcard.
      include = [
        "*.x86_64-linux.*"
        # "*.aarch64-linux.*"
        "packages.x86_64-linux.*"
        "packages.aarch64-linux.*"
        "devShells.x86_64-linux.*"
        # "nixosConfigurations.*"
      ];

      # What builds to exclude. Follows the same format as `include`. This is
      # applied *after* the `include`. Thus, if something matches both the
      # `include` and the `exclude`, it will be excluded.
      exclude = [
        "darwinConfigurations.*"
        "*.aarch64-darwin.*"
        "*.x86_64-darwin.*"
      ];
    };
  };
}
