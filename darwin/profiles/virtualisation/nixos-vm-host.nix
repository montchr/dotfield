{
  nix.settings = {
    # substituters = ["https://macos-builder.cachix.org"];
    trusted-substituters = ["https://macos-builder.cachix.org"];
    trusted-public-keys = ["macos-builder.cachix.org-1:HPWcq59/iyqQz6HEtlO/kjD/a7ril0+/XJc+SZ2LgpI="];
    # FIXME: error: The option `nix.settings.builders` is defined both null and not null, in
    # `/nix/store/2jbm1fsg3bg6jkkss200ryfrafdxd11q-source/darwin/profiles/virtualisation/nixos-vm-host.nix'
    # and `/nix/store/g0wrd3s71gh9ns7vjz6gf1gp6n6w2xf4-source/modules/nix'.
    # builders = "ssh-ng://builder@localhost aarch64-linux /etc/nix/nixbld_ed25519 - - - - ${vmKey}";
    builders-use-substitutes = true;
  };
}
