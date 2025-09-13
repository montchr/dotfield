{
  users.cdom.aspects.core.home = {
    programs.git = {
      aliases = {
        snapshot = ''!git stash save "snapshot: $(date)" && git stash apply "stash@{0}"'';
      };
      extraConfig = {
        push.default = "current";
        apply.whitespace = "nowarn";
        pull.rebase = true;
        merge.conflictstyle = "diff3";
        merge.tool = "ediff";
        diff = {
          algorithm = "histogram";
          colorMoved = "dimmed-zebra";
          tool = "ediff";
        };
      };
    };
  };
}
