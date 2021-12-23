{ config, lib, pkgs, ... }:

{
  # Allow VirtualBox to manage host networks.
  environment.etc."vbox/networks.conf".text = "* 0.0.0.0/0 ::/0";

  environment.systemPackages = with pkgs; [ dnsmasq ];
  services.dnsmasq = {
    enable = false;
    addresses = {
      # Vagrant boxes.
      http = "192.168.50.4";
      test = "192.168.50.4";
    };
  };
}
