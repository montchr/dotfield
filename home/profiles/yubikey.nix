{ pkgs, ... }:
{
  home.packages = [
    # XXX: build failure for dependency python3.12-pyscard-2.1.1
    #  pkgs.yubikey-manager
  ];
  services.gpg-agent.enableScDaemon = true;
}
