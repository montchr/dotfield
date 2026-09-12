{
  aspects.workstation.nixos = {
    programs.nh = {
      enable = true;
      flake = "/etc/nixos";

      # Manage store cleanup for workstations.  Unlike `nix.gc` this
      # also prunes per-user home-manager generations.
      clean.enable = true;
      clean.extraArgs = "--keep 5 --keep-since 14d";
    };
  };
}
