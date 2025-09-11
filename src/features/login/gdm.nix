{
  aspects.login__gdm.nixos = {
    services.displayManager.gdm = {
      enable = true;
      wayland = true;
      autoSuspend = false;
    };
  };
}
