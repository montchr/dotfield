{
  hosts.nixos.boschic = {
    services.tailscale.enable = true;

    networking.networkmanager.enable = true;
    networking.usePredictableInterfaceNames = false;

    # Whether to enable DHCP on each ethernet and wireless interface. In
    # case of scripted networking (the default) this is the recommended
    # approach. When using systemd-networkd it's still possible to use
    # this option, but it's recommended to use it in conjunction with
    # explicit per-interface declarations with
    # `networking.interfaces.<interface>.useDHCP`.
    networking.useDHCP = false;
    # networking.interfaces.enp9s0.useDHCP = lib.mkDefault true;
    # networking.interfaces.ts0.useDHCP = lib.mkDefault true;
    # networking.interfaces.wlp7s0.useDHCP = lib.mkDefault true;

    networking.firewall.enable = true;
  };
}
