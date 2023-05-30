{
  flake,
  pkgs,
  ...
}: let
  l = inputs.nixpkgs.lib // builtins;
in {
  home.packages = [pkgs.blesh];
  xdg.configFile."blesh/init.sh".source = ./init.sh;

  # The interactive shell check here happens again immediately after the output
  # of `bashrcExtra`. However, ble.sh needs to be initalized as early as
  # possible, and there is currently no other place to do so after the
  # hard-coded interactive check and the next config value output
  # (at the time of writing, that's the `historyControlStr`).
  programs.bash.bashrcExtra = l.mkAfter ''
    [[ $- == *i* ]] && source ${pkgs.blesh}/share/blesh/ble.sh --noattach
  '';

  programs.bash.initExtra = l.mkAfter ''
    [[ ''${BLE_VERSION-} ]] && ble-attach
  '';
}
