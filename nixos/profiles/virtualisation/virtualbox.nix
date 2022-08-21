{
  config,
  lib,
  pkgs,
  ...
}: let
  inherit (pkgs.stdenv.hostPlatform) isDarwin isLinux;
in
  lib.mkMerge [
    (lib.mkIf isLinux {
      virtualisation.virtualbox.host.enable = true;
      virtualisation.virtualbox.host.enableWebService = true;
      virtualisation.virtualbox.host.enableExtensionPack = true;
    })
    (lib.mkIf isDarwin {
      # Allow VirtualBox to manage host networks.
      # TODO: still necessary?
      # environment.etc."vbox/networks.conf".text = "* 0.0.0.0/0 ::/0";

      environment.systemPackages = with pkgs; [dnsmasq];
      services.dnsmasq = {
        # enable = true;
        enable = false;
        # FIXME: causes build failure even on nixos, where option is not available...
        # addresses = {
        #   # FIXME: use correct IPs
        #   # http = "192.168.50.4";
        #   # test = "192.168.50.4";
        # };
      };
    })
  ]
