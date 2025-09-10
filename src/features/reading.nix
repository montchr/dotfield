{
  aspects.workstation.home =
    { pkgs, ... }:
    {
      home.packages = [
        pkgs.calibre

        pkgs.mcomix
      ];
    };
}
