# Firefox must be build with native messaging hosts in the system scope because
# individual home-manager configurations do not have the capability to manage
# system-level integrations.
{ pkgs, ... }:
{
  programs.firefox = {
    enable = true;
    nativeMessagingHosts.packages = [
      pkgs.bukubrow
      pkgs.tridactyl-native
      pkgs.passff-host
    ];
  };
}
