{ pkgs, ... }:
{
  imports = [ ./libvirt.nix ];
  programs.dconf.enable = true;
  environment.systemPackages = with pkgs; [ virt-manager ];
}
