{
  aspects.graphical.home = {
    programs.ghostty = {
      enable = true;
      enableBashIntegration = true;
      enableFishIntegration = true;
      installBatSyntax = true;
    };
    dconf.settings."org/cinnamon/desktop/applications/terminal".exec = "ghostty";
  };
}
