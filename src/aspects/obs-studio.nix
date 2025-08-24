{
  dotfield.aspects.workstation.nixos = {
    programs.obs-studio = {
      enable = true;
      enableVirtualCamera = true;
    };
  };

  dotfield.aspects.workstation.home = {
    programs.obs-studio.enable = true;
  };
}
