{
  dotfield.users.cdom.features.workstation.home =
    { pkgs, ... }:
    {
      home.packages = [
        pkgs.protonvpn-gui
      ];
    };
}
