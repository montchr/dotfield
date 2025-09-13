{ self, lib, ... }:
let
  inherit (self.lib.shell) cmd;
in
{
  users.cdom.aspects.core.home = (
    { pkgs, ... }:
    let
      dirPreviewCommand = "${lib.getExe pkgs.eza} --tree {} | head -n 200";
    in
    {
      programs.fzf = {
        defaultOptions = [
          "--ansi"
          "--reverse"
          "--border"
          "--inline-info"
          "--color=16"
        ];
        fileWidgetCommand = cmd pkgs.fd {
          type = "file";
          hidden = true;
          follow = true;
          exclude = [
            ".git"
            ".devenv"
            ".direnv"
            "node_modules"
            "vendor"
          ];
        };
        # TODO: use `bat` -- see `igr` package source for example (doesn't include `head`-like tho)
        fileWidgetOptions = [ "--preview 'head {}'" ];
        changeDirWidgetCommand = cmd pkgs.fd { "type" = "directory"; };
        changeDirWidgetOptions = [
          "--tiebreak=index"
          "--preview '${dirPreviewCommand}'"
        ];
        historyWidgetOptions = [
          "--sort"
          "--exact"
        ];
      };
    }
  );
}
