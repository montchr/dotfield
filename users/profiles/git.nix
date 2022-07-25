{
  config,
  lib,
  pkgs,
  inputs,
  ...
}: let
  inherit (lib) getAttr attrNames;
  inherit (config.lib.dotfield.whoami) email fullName pgpPublicKey;

  enableSigning =
    config.programs.gpg.enable
    && config.services.gpg-agent.enable
    && "" != pgpPublicKey;
in {
  home.packages = with pkgs; [
    ediff-tool
    exiftool # EXIF diff handler
    git-cliff
    git-submodule-rewrite
    gitAndTools.hub
    gitAndTools.gh
    gitAndTools.tig

    # Identify the largest files in a git repo's history.
    #
    # Even after committing the deletion of a file, it will remain in git
    # history forever. This script allows for the identification of such files,
    # sorted from smallest to largest.
    #
    # via: https://stackoverflow.com/a/42544963
    (writeShellScriptBin "git-hls-by-size" ''
      git rev-list --objects --all \
        | git cat-file --batch-check='%(objecttype) %(objectname) %(objectsize) %(rest)' \
        | sed -n 's/^blob //p' \
        | sort --numeric-sort --key=2 \
        | cut -c 1-12,41- \
        | $(command -v gnumfmt || echo numfmt) --field=2 --to=iec-i --suffix=B --padding=7 --round=nearest
    '')
  ];

  programs.git = {
    enable = true;
    package = pkgs.gitAndTools.gitFull;

    userEmail = email;
    userName = fullName;

    signing = lib.mkIf enableSigning {
      key = pgpPublicKey;
      signByDefault = true;
    };

    delta = {
      enable = true;
      options = {
        line-numbers = true;
        navigate = true;
        keep-plus-minus-markers = true;
      };
    };

    ignores = [
      ".yarn"
      "node_modules"

      # Logs and databases
      "*.sql"
      "*.sqlite"
      ".log"

      # OS or Editor files
      "._*"
      ".DS_Store"
      ".DS_Store?"
      "ehthumbs.db"
      "Thumbs.db"
      ".tern-project"

      # Files that might appear on external disks
      ".Spotlight-V100"
      ".Trashes"

      # Always-ignore extensions
      "*~"
      "*.err"
      "*.orig"
      "*.pyc"
      "*.rej"
      "*.sw?"
      "*.vi"
      "*.bak"

      # Credentials and Sensitive Info
      ".env"
      ".direnv"
      ".scratch"
      "*localrc"
      "*.local"
    ];

    extraConfig = lib.mkMerge [
      {
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
          ediff.cmd = "${pkgs.ediff-tool}/bin/ediff-tool $LOCAL $REMOTE";
          vscode.cmd = "code --wait --diff $LOCAL $REMOTE";
        };

        mergetool = {
          prompt = false;
          ediff.cmd = "${pkgs.ediff-tool}/bin/ediff-tool $LOCAL $REMOTE $MERGED";
          vscode.cmd = "code --wait $MERGED";
        };
        ##: }}
      }

      (lib.mkIf pkgs.stdenv.hostPlatform.isDarwin {
        # TODO: still necessary?
        credential.helper = "osxkeychain";
      })
    ];
  };

  programs.gh.enable = true;
  programs.gh.settings.git_protocol = "ssh";

  xdg.configFile."git/templates".source = "${pkgs.dotfield-config}/git/templates";
}
