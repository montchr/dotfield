{
  config,
  pkgs,
  packages,
  inputs,
  self,
  ...
}: let
  inherit (config) theme;
  l = inputs.nixpkgs.lib // builtins;

  # procSub = s: "<(${s})";

  packageCommand = pkg: args: (l.getExe pkg) + " " + (l.cli.toGNUCommandLineShell {} args);

  find = packageCommand pkgs.fd;
  findFiles = args: find (args // {type = "f";});
  findDirs = args: find (args // {type = "d";});
  # list = packageCommand pkgs.exa;

  dirPreviewCommand = l.getExe pkgs.exa + " --tree {} | head -n 200";
in {
  home.packages = [packages.fre packages.igr];

  programs.fzf = {
    enable = true;
    # defaultCommand = "${findFiles {}} 2>/dev/null";
    defaultOptions =
      ["--ansi" "--reverse" "--border" "--inline-info"]
      ++ (l.optional theme.enable "--color=16");

    ##: --- files ---

    fileWidgetCommand = findFiles {
      hidden = true;
      follow = true;
      exclude = [".git" ".direnv" ".std" "node_modules" "vendor"];
    };
    fileWidgetOptions = ["--preview 'head {}'"];

    ##: --- directories ---

    changeDirWidgetCommand = findDirs {};
    changeDirWidgetOptions = [
      "--tiebreak=index"
      "--preview '${dirPreviewCommand}'"
    ];

    ##: --- history ---

    historyWidgetOptions = ["--sort" "--exact"];
  };
}
