{ config, pkgs, ... }: {
  imports = [ ../modules/darwin ];
  nixpkgs.system = "x86_64-darwin";
  networking.dns = [ ];
}
