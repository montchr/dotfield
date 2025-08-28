# Firefox must be built with native messaging hosts in the system scope because
# individual home-manager configurations do not have the capability to manage
# system-level integrations.
{ pkgs, ... }:
{
  programs.firefox = {
    enable = true;
    nativeMessagingHosts.packages = [
      pkgs.tridactyl-native
      pkgs.passff-host
    ];
  };
}
