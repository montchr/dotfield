{
  flake.modules.nixos.vpn =
    { pkgs, ... }:
    {
      environment.systemPackages = with pkgs; [ protonvpn-gui ];
    };
}
