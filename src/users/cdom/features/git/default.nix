flake@{ moduleWithSystem, ... }:
let
  inherit (flake.config.dotfield.meta.users.cdom) whoami;
in
{
  dotfield.baseline.home = {
    programs.git = {
      userEmail = whoami.email;
      userName = whoami.name;
    };
  };

  dotfield.users.cdom.aspects.development.home = moduleWithSystem (
    perSystem@{ config, ... }:
    home@{ config, pkgs, ... }:
    let
      inherit (flake.config.dotfield.meta.users.${config.home.username}) whoami;
    in
    {
      home.packages = [
        pkgs.exiftool # <- EXIF diff handler
      ];

      programs.git.difftastic = {
        enable = true;
        # display = "inline";
      };

      programs.git.extraConfig = {
        github.user = whoami.github;
        merge.conflictstyle = "diff3";
        merge.tool = "ediff";
        diff = {
          algorithm = "histogram";
          exif.textconv = "${pkgs.exiftool}/bin/exiftool";
          colorMoved = "dimmed-zebra";
          tool = "ediff";
        };

        # Result: <short-sha> <commit-message> (<pointer-names>) -- <commit-author-name>; <relative-time>
        pretty.nice = "%C(yellow)%h%C(reset) %C(white)%s%C(cyan)%d%C(reset) -- %an; %ar";

        ## remotes
        fetch.recurseSubmodules = true;
        push.default = "current";
        apply.whitespace = "nowarn";
        pull.rebase = true;
      };

      programs.gh.settings.git_protocol = "ssh";
    }
  );
}
