# PragmataPro lives in a private repo (for licensing reasons). That makes it
# easier to exclude from CI builds so that SSH keys aren't required.
{
  config,
  lib,
  pkgs,
  ...
}: let
  pragmataPro = pkgs.callPackage ../../packages/fonts/common/pragmatapro.nix {};
in {
  fonts.fonts = [pragmataPro];
}
