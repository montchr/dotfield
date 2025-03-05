{ pkgs, ... }:
{
  programs.librewolf = {
    enable = true;
    package = pkgs.librewolf.override {
      nativeMessagingHosts = [
        pkgs.tridactyl-native
        pkgs.passff-host
      ];
    };
    settings = {
      "identity.fxaccounts.enabled" = true;
      "privacy.clearOnShutdown.history" = false;
      "privacy.clearOnShutdown.downloads" = false;
      # NOTE: Inverse boolean.
      "webgl.disabled" = false;
    };
  };
}
