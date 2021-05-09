{ config, inputs, ... }: {
  imports = [ ../modules/darwin ];
  networking.hostName = "HodgePodge";
}
