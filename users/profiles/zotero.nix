{ config, lib, pkgs, ... }:
let
  inherit (pkgs.stdenv.hostPlatform) isDarwin;
in
{
  home.packages = with pkgs; [
    # FIXME: how is this unsupported on darwin? prob cos macOS apps don't install well with nix
    (lib.mkIf (!isDarwin) zotero)
  ];
}
