{ pkgs, lib, config, ... }:

let
  dotfield = config.dotfield;

  cfg = config.my.modules.git;
  # cfgDir = "${dotfield.configDir}/git";

in {
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
          gitAndTools.transcrypt
          gitAndTools.delta
          gitAndTools.hub
          gitAndTools.gh
          gitAndTools.tig
          universal-ctags
          exiftool
        ];
      };

      my.hm = {
        configFile = {
          "git/config-nix" = with config.my; {
            text = ''
              ; ${nix_managed}
              ; vim: ft=gitconfig

              [user]
              ${optionalString (name != "") "  name = ${name}"}
              ${optionalString (email != "") "  email = ${email}"}
                useconfigonly = true

              ${optionalString (github_username != "") ''
                [github]
                  username = ${github_username}
              ''}

              [gpg]
                program = ${pkgs.gnupg}/bin/gpg

              [diff "exif"]
                textconv = ${pkgs.exiftool}/bin/exiftool

              ${optionalString (pkgs.stdenv.isDarwin) ''
                [diff "plist"]
                  textconv = plutil -convert xml1 -o -
                [credential]
                  helper = "osxkeychain"
              ''}
            '';
          };

          # "git" = {
          #   recursive = true;
          #   source = builtins.toPath /. cfgDir;
          # };

          # "tig" = {
          #   recursive = true;
          #   source = builtins.toPath /. "${dotfield.configDir}/tig";
          # };
        };
      };
    };
}
