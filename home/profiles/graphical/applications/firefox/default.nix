hmArgs@{ flake, pkgs, ... }:
let
  inherit (flake.lib) mimetypes;
in
{
  imports = [ ./profiles.nix ];

  programs.firefox = {
    enable = true;
    package =
      if (hmArgs.osConfig.programs.firefox.enable or false) then
        (hmArgs.osConfig.programs.firefox.package or pkgs.firefox)
      else
        pkgs.firefox;
  };
}
