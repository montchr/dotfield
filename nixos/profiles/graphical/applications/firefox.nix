{ pkgs, ... }:
{
  programs.firefox.enable = true;

  # TODO: only include these when some home-manager user needs them (see apparat lib function)
  programs.firefox.nativeMessagingHosts.packages = [
    pkgs.bukubrow
    pkgs.tridactyl-native
    pkgs.passff-host
  ];
}
