{ config, inputs, ... }: {
  imports = [ ../modules/darwin ];

  my = {
    username = "montchr";
    email = "chris@alley.co";
    website = "https://alley.co/";
  };

  networking.hostName = "alleymon";

  environment.systemPackages = with pkgs; [
    dnsmasq
  ];

  services.dnsmasq = {
    enable = true;
    addresses = {
      # Vagrant boxes.
      http = "192.168.50.4";
      test = "192.168.50.4";
    };
  };
}
