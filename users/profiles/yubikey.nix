{ config, lib, pkgs, ... }:

{
  home.packages = with pkgs; [
    yubikey-manager
    yubikey-personalization
  ];

  programs.gpg.scdaemonSettings = {
    disable-ccid = true;
    # TODO: is this still necessary? only mention in DrDuh guide is for Windows
    reader-port = "Yubico Yubi";
  };

  services.gpg-agent.enableScDaemon = true;
}
