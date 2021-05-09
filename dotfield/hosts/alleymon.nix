{ config, inputs, ... }: {
  imports = [ ../modules/darwin ];

  my = {
    username = "montchr";
    email = "chris@alley.co";
    website = "https://alley.co/";
  };

  networking.hostName = "alleymon";
}
