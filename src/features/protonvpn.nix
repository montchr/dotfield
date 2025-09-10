{
  aspects.workstation.nixos =
    { pkgs, ... }:
    {
      environment.systemPackages = with pkgs; [ protonvpn-gui ];
    };
}
