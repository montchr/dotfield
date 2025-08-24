{
  dotfield.users.cdom.aspects.workstation.home =
    { pkgs, ... }:
    {
      home.packages = [
        pkgs.protonvpn-gui
      ];
    };
}
