{
  dotfield.home =
    { config, ... }:
    {
      programs.pandoc.enable = true;
      home.packages = [ config.programs.pandoc.finalPackage ];
    };
}
