{ pkgs, lib, config, ... }:

let
  inherit (lib) getAttr attrNames;

  cfg = config.my.modules.git;
  configDir = "${config.dotfield.configDir}/git";

  scripts = with pkgs; {
    submoduleRewrite = (writeScriptBin "git-submodule-rewrite"
      (builtins.readFile "${configDir}/bin/git-submodule-rewrite"));
  };

  userScripts = (builtins.map
    (key: getAttr key scripts)
    (attrNames scripts));

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

      my.env = { GIT_EDITOR = "$EDITOR"; };

      my.user.packages = with pkgs; [
        gitAndTools.transcrypt
        gitAndTools.delta
        gitAndTools.hub
        gitAndTools.gh
        gitAndTools.tig
        universal-ctags
        exiftool
      ] ++ userScripts;

      my.hm.configFile = {

        "git/config" =
          let
            inherit (config.my)
              name
              email
              githubUsername
              keys
              nix_managed
              xdgPaths
              ;

            signingKey = keys.pgp;

            ediffTool =
              "${config.my.modules.editors.emacs.ediffTool.package}/bin/ediff-tool";
          in
          {

            text = ''
              ; ${nix_managed}

              ${builtins.readFile "${configDir}/config"}

              [user]
                name = ${name}
                email = ${email}
                useconfigonly = true
                ${optionalString (signingKey != "") ''
                  signingkey = ${signingKey}
                ''}

              ${optionalString (githubUsername != "") ''
                [github]
                  username = ${githubUsername}
              ''}

              [gpg]
                program = ${pkgs.gnupg}/bin/gpg

              [init]
                templateDir = ${xdgPaths.config}/git/templates

              [diff "exif"]
                textconv = ${pkgs.exiftool}/bin/exiftool

              # Diff/Merge Tools
              [difftool]
                prompt = false
              [mergetool]
                prompt = false
              [mergetool "ediff"]
                cmd = ${ediffTool} $LOCAL $REMOTE $MERGED
              [mergetool "vscode"]
                  cmd = code --wait $MERGED
              [difftool "vscode"]
                  cmd = code --wait --diff $LOCAL $REMOTE
              [difftool "ediff"]
                  cmd = ${ediffTool} $LOCAL $REMOTE
              [diff]
                  colorMoved = default
                  tool = ediff
              [merge]
                  tool = ediff

              ${optionalString (pkgs.stdenv.isDarwin) ''
                [diff "plist"]
                  textconv = plutil -convert xml1 -o -
                [credential]
                  helper = "osxkeychain"
              ''}
            '';
          };

        "git/ignore".source = "${configDir}/ignore";

        "git/templates" = {
          source = "${configDir}/templates";
          recursive = true;
        };
      };
    };
}
