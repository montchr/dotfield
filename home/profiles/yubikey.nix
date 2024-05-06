{ pkgs, ... }:
{
  home.packages = [ pkgs.yubikey-manager ];
  services.gpg-agent.enableScDaemon = true;
}
