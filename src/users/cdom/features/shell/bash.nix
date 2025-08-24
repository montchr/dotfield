{ lib, ... }:
{
  dotfield.users.cdom.aspects.home.home = {
    programs.bash = {
      historyControl = [
        "erasedups"
        "ignorespace"
      ];
      initExtra = lib.mkAfter ''
        # Must C-d at least twice to close shell.
        export IGNOREEOF=1
      '';
    };
  };
}
