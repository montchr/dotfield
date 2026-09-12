{
  aspects.workstation.nixos = {
    programs.nh = {
      enable = true;
      flake = "/etc/nixos";
    };
  };
}
