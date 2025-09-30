{
  users.cdom.aspects.core.home = {
    programs.git.extraConfig = {
      apply.whitespace = "nowarn";
      pull.rebase = true;
      merge.tool = "ediff";
      diff = {
        algorithm = "histogram";
        colorMoved = "dimmed-zebra";
        tool = "ediff";
      };
    };
  };
}
