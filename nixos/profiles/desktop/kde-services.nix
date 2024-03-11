{
  programs.kdeconnect.enable = true;

  home-manager.sharedModules = [
    {
      services.kdeconnect = {
        enable = true;
        indicator = true;
      };
    }
  ];
}
