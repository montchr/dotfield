{
  lib,
  pkgs,
  config,
  ...
}: let
  inherit (pkgs.stdenv.hostPlatform) isDarwin;
in {
  home.packages = [pkgs.blesh];

  # <https://github.com/akinomyoga/ble.sh?tab=readme-ov-file#14-user-settings-blerc>
  xdg.configFile."blesh/init.sh".source = ./init.sh;

  programs.bash.bashrcExtra = lib.mkAfter ''
    [[ $- == *i* ]] && source ${pkgs.blesh}/share/blesh/ble.sh --noattach
  '';

  programs.bash.initExtra = lib.mkAfter ''
    [[ ''${BLE_VERSION-} ]] && ble-attach
  '';
}
