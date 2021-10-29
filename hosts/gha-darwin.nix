{ config, pkgs, ... }: {
  imports = [ ../modules/darwin ];
  networking.hostName = "gha-darwin";
  nixpkgs.system = "x86_64-darwin";
  networking.dns = [ ];
}
