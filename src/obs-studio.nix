{
  flake.modules.nixos.graphical = {
    programs.obs-studio = {
      enable = true;
      enableVirtualCamera = true;
    };
  };
}
