{
  aspects.blesh.home =
    { lib, pkgs, ... }:
    {
      home.packages = [ pkgs.blesh ];

      programs.bash.bashrcExtra = lib.mkAfter ''
        [[ $- == *i* ]] && source ${pkgs.blesh}/share/blesh/ble.sh --noattach
      '';

      programs.bash.initExtra = lib.mkAfter ''
        [[ ''${BLE_VERSION-} ]] && ble-attach
      '';
    };
}
