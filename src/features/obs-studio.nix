{
  dotfield.features.workstation.nixos = {
    programs.obs-studio = {
      enable = true;
      enableVirtualCamera = true;
    };
  };

  dotfield.features.workstation.home = {
    programs.obs-studio.enable = true;
  };
}
