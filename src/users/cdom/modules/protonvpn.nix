{
  dotfield.modules.workstation.home =
    { pkgs, ... }:
    {
      home.packages = [
        pkgs.protonvpn-gui
      ];
    };
}
