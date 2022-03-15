{
  config,
  osConfig,
  lib,
  pkgs,
  ...
}: let
  inherit (osConfig) my;
  inherit (lib) getAttr attrNames;
  inherit (pkgs.stdenv.targetPlatform) isDarwin;

  configDir = "${osConfig.dotfield.configDir}/git";

  scripts = {
    submoduleRewrite =
      pkgs.writeScriptBin "git-submodule-rewrite"
      (builtins.readFile "${configDir}/bin/git-submodule-rewrite");
  };

  userScripts =
    builtins.map
    (key: getAttr key scripts)
    (attrNames scripts);

  ediffTool = "${pkgs.dotfield.ediffTool}/bin/ediff-tool";
in {
  home.packages = with pkgs;
    [
      gitAndTools.hub
      gitAndTools.gh
      gitAndTools.tig
      # For EXIF diff handling
      exiftool
      git-cliff
    ]
    ++ userScripts;

  programs.git = {
    enable = true;
    package = pkgs.gitAndTools.gitFull;

    userEmail = my.email;
    userName = my.name;

    signing = {
      key = my.keys.pgp;
      signByDefault = true;
    };

    delta = {
      enable = true;
      options = {
        line-numbers = true;
        navigate = true;
      };
    };

    extraConfig =
      {
        github.user = my.usernames.github;
        gitlab.user = my.usernames.gitlab;
        sourcehut.user = my.usernames.sourcehut;

        init.defaultBranch = "main";

        # Environment variables will not be expanded -- this requires a path.
        init.templateDir = "${config.xdg.configHome}/git/templates";

        # Result: <short-sha> <commit-message> (<pointer-names>) -- <commit-author-name>; <relative-time>
        pretty.nice = "%C(yellow)%h%C(reset) %C(white)%s%C(cyan)%d%C(reset) -- %an; %ar";

        ##: Remotes {{
        fetch.recurseSubmodules = true;
        push.default = "current";
        apply.whitespace = "nowarn";
        # Only enable this on a per-repo basis.
        pull.rebase = false;
        ##: }}

        ##: Diff/Merge Tools {{
        rerere.enabled = true;
        merge.conflictstyle = "diff3";
        merge.tool = "ediff";

        diff = {
          algorithm = "minimal";
          exif.textconv = "${pkgs.exiftool}/bin/exiftool";
          # colorMoved = "default";
          tool = "ediff";
          # `plutil` is a darwin utility
          plist.textconv = "plutil -convert xml1 -o -";
        };

        difftool = {
          prompt = false;
          ediff.cmd = "${ediffTool} $LOCAL $REMOTE";
          vscode.cmd = "code --wait --diff $LOCAL $REMOTE";
        };

        mergetool = {
          prompt = false;
          ediff.cmd = "${ediffTool} $LOCAL $REMOTE $MERGED";
          vscode.cmd = "code --wait $MERGED";
        };
        ##: }}
      }
      // (lib.optionals isDarwin {
        credential.helper = "osxkeychain";
      });
  };

  xdg.configFile = {
    "git/ignore".source = "${configDir}/ignore";
    "git/templates".source = "${configDir}/templates";
  };
}
