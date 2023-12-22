hmArgs @ {pkgs, ...}: let
  inherit (pkgs.stdenv.hostPlatform) isDarwin;
in {
  imports = [
    ./profiles.nix
    ../os-specific/darwin/firefox-profile-hack.nix
  ];
  programs.firefox = {
    enable = true;
    package =
      # TODO: darwin workaround might no longer be necessary since home-manager apps are usable on darwin now
      if isDarwin
      then pkgs.runCommand "firefox-0.0.0" {} "mkdir $out"
      else if (hmArgs.osConfig.programs.firefox.enable or false)
      then (hmArgs.osConfig.programs.firefox.package or pkgs.firefox)
      else pkgs.firefox;
  };
}
