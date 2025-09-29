{
  aspects.graphical.home =
    {
      config,
      pkgs,
      lib,
      ...
    }:
    {
      programs.fuzzel.enable = true;
      programs.fuzzel.settings.main = {
        terminal = lib.mkDefault "ghostty";
      };
      home.packages = [ pkgs.fuzzel ];
    };
}
