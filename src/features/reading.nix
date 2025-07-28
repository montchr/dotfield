{
  dotfield.modules.workstation.home =
    { pkgs, ... }:
    {
      home.packages = [
        pkgs.calibre
        pkgs.mcomix
      ];
    };
}
