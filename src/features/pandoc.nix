{
  aspects.workstation.home =
    { config, ... }:
    let
      cfg = config.programs.pandoc;
    in
    {
      programs.pandoc.enable = true;
      home.packages = [ cfg.finalPackage ];
    };
}
