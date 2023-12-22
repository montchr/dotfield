{
  config,
  lib,
  pkgs,
  ...
}: let
  inherit (pkgs.stdenv.hostPlatform) isLinux;
in {
  # <https://github.com/hluk/CopyQ>
  # alternatively:
  # services.cliphist.enable = true;
  # copyq has a whole bunch of open issues, but it's popular and many of them are mswin-specifc
  services.copyq.enable = true;

  home.packages =
    [config.services.copyq.package]
    ++ lib.optionals isLinux [pkgs.wl-clipboard];
}
