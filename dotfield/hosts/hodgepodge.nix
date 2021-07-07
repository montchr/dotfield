{ config, inputs, ... }: {
  imports = [ ../modules/darwin ];
  networking.hostName = "HodgePodge";

  my = {
    username = "cdom";
    email = "chris@cdom.io";
    website = "https://github.com/montchr/";
  };
}
