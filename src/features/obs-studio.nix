{
  dotfield.modules.workstation.nixos = {
    programs.obs-studio = {
      enable = true;
      enableVirtualCamera = true;
    };
  };

  dotfield.modules.workstation.home = {
    programs.obs-studio.enable = true;
  };
}
