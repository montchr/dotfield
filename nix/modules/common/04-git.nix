{ pkgs, lib, config, ... }:

let
  cfg = config.my.modules.git;
in
{
  options = with lib; {
    my.modules.git = {
      enable = mkEnableOption ''
        Whether to enable git module
      '';
    };
  };

  config = with lib;
    mkIf cfg.enable {
      environment.systemPackages = with pkgs; [ git ];

      my.user = {
        packages = with pkgs; [
          # TODO: check out exiftool sometime
          # exiftool
          gitAndTools.delta
          gitAndTools.gh
          gitAndTools.hub
          gitAndTools.transcrypt
          universal-ctags
        ];
      };

      my.hm.file = {
        # TODO: use nix to manage git config
        ".config/git/config-nix" = with config.my; {
          text = ''
            ; ${nix_managed}
            ; vim: ft=gitconfig

            [user]
            ${optionalString (name != "") "  name = ${name}"}
            ${optionalString (email != "") "  email = ${email}"}
              useconfigonly = true

            ${optionalString (github_username != "") ''
              [github]
                username = ${github_username}''}

            [gpg]
              program = ${pkgs.gnupg}/bin/gpg

            # [diff "exif"]
            #   textconv = ${pkgs.exiftool}/bin/exiftool

            ${optionalString (pkgs.stdenv.isDarwin) ''
              [diff "plist"]
                textconv = plutil -convert xml1 -o -''}
          '';
        };

        ".config/git" = {
          recursive = true;
          source = ../../../config/git;
        };
      };
    };
}
