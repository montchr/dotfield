{
  config,
  lib,
  pkgs,
  ...
}:
let inherit (pkgs.stdenv.hostPlatform) isDarwin;
inherit (lib) mkIf;
in
mkIf (!isDarwin) {
  programs.chromium.enable = true;
}
