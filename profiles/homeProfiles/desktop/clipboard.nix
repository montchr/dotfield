{
  config,
  lib,
  pkgs,
  ...
}: let
  inherit (pkgs.stdenv.hostPlatform) isLinux;
in {
  services.cliphist.enable = true;

  home.packages =
    [
      config.services.cliphist.package
    ]
    ++ lib.optionals isLinux [
      pkgs.wl-clipboard

      # via <https://github.com/sentriz/cliphist#picker-examples>
      (pkgs.writeShellApplication {
        name = "clipsel";
        runtimeInputs = [pkgs.cliphist pkgs.fzf pkgs.wl-clipboard];
        text = ''
          cliphist list | fzf | cliphist decode | wl-copy
        '';
      })
    ];
}
