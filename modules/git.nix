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

      my.hm.xdg.configFile =
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

          "git/config".text = ''
            [user]
              name = ${name}
              email = ${email}
              useconfigonly = true
              signingkey = ${signingKey}

            [github]
              user = ${githubUsername}

            [gpg]
              program = ${pkgs.gnupg}/bin/gpg

            [init]
              templateDir = ${xdgPaths.config}/git/templates
              defaultBranch = main

            [core]
                pager = delta

            [help]
                autocorrect = 0
            [pretty]
                # tut: http://gitimmersion.com/lab_10.html
                # ref: http://linux.die.net/man/1/git-log
                # Result: <short-sha> <commit-message> (<pointer-names>) -- <commit-author-name>; <relative-time>
                nice = "%C(yellow)%h%C(reset) %C(white)%s%C(cyan)%d%C(reset) -- %an; %ar"
            [commit]
                gpgsign = true
            [fetch]
                recurseSubmodules = true
            [pull]
                rebase = true
            [push]
                # See `git help config` (search for push.default)
                # for more information on different options of the below setting.
                default = current
            [rerere]
                enabled = true
            [apply]
                whitespace = nowarn


            # Diff/Merge Tools
            [delta]
                line-numbers = true
                navigate = true
            [merge]
              conflictstyle = diff3
            [interactive]
                diffFilter = delta --color-only
            [diff "exif"]
              textconv = ${pkgs.exiftool}/bin/exiftool
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

            [diff "plist"]
              textconv = plutil -convert xml1 -o -
            [credential]
              helper = "osxkeychain"
          '';

          "git/ignore".source = "${configDir}/ignore";

          "git/templates" = {
            source = "${configDir}/templates";
            recursive = true;
          };
        };
    };
}
