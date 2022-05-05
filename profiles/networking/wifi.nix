{
  config,
  lib,
  pkgs,
  ...
}:
lib.mkIf (!pkgs.stdenv.hostPlatform.isDarwin) {
  networking.wireless = {
    # Defines environment variables for wireless network passkeys.
    environmentFile = config.age.secrets."wireless.env".path;
    networks.bortHole.psk = "@PSK_BORTHOLE@";
  };
}
