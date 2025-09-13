{
  aspects.workstation.home =
    { pkgs, ... }:
    {
      home.packages = with pkgs; [ protonvpn-gui ];
    };
}
