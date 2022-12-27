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

  procSub = s: "<(${s})";
  packageCommand = pkg: args: (l.getExe pkg) + " " + (l.cli.toGNUCommandLineShell {} args);

  find = packageCommand pkgs.fd;
  findFiles = args: find (args // {type = "f";});
  findDirs = args: find (args // {type = "d";});
  # list = packageCommand pkgs.exa;
  freCommand = packageCommand packages.fre {sorted = true;};

  toSourcesCommand = srcs: "command cat " + (l.concatMapStringsSep " " procSub srcs);
  toSourcesCommand' = srcs: toSourcesCommand ([freCommand] ++ srcs);

  commonWidgetOpts = [
    "--tiebreak=index"
    # `fre` will take care of sorting in widgets
    "--no-sort"
  ];

  dirPreviewCommand = l.getExe pkgs.exa + " --tree {} | head -n 200";
in {
  imports = [./igr.nix];

  programs.fzf = {
    enable = true;
    defaultCommand = "${findFiles {}} 2>/dev/null";
    defaultOptions =
      ["--ansi" "--reverse"]
      ++ (l.optional theme.enable "--color=16");

    ##: --- files ---

    fileWidgetCommand = toSourcesCommand' [
      (findFiles {
        hidden = true;
        follow = true;
        exclude = [".git"];
      })
    ];
    fileWidgetOptions = commonWidgetOpts ++ ["--preview 'head {}'"];

    ##: --- directories ---

    changeDirWidgetCommand = toSourcesCommand' [
      (findDirs {})
      ((findDirs {}) + " . ~")
    ];
    changeDirWidgetOptions = commonWidgetOpts ++ ["--preview '${dirPreviewCommand}'"];

    ##: --- history ---

    historyWidgetOptions = ["--sort" "--exact"];
  };

  ###: --- `fre` configuration -------------------------------------------------

  home.packages = [packages.fre];
  programs.bash.initExtra = ''
    __fre_pwd() {
      builtin pwd -L
    }
    __fre_oldpwd="$(__fre_pwd)"
    __fre_hook() {
      \builtin local -r retval="$?"
      \builtin local pwd_tmp
      pwd_tmp="$(__fre_pwd)"
      if [[ $__fre_oldpwd != "$pwd_tmp" ]]; then
        __fre_oldpwd="$pwd_tmp"
        \command fre --add "$__fre_oldpwd"
      fi
      return "$retval"
    }
    if [[ ''${PROMPT_COMMAND:=} != *'__fre_hook'* ]]; then
      PROMPT_COMMAND="__fre_hook;''${PROMPT_COMMAND#;}"
    fi
  '';
  programs.zsh.initExtra = ''
    __fre_pwd() {
      \builtin pwd -L
    }
    __fre_hook() {
      fre --add "$(__fre_pwd)"
    }
    if [[ ''${precmd_functions[(Ie)__fre_hook]:-} -eq 0 ]] && [[ ''${chpwd_functions[(Ie)__fre_hook]:-} -eq 0 ]]; then
      chpwd_functions+=(__fre_hook)
    fi
  '';
  programs.fish.shellInit = ''
    function __fre_pwd
      builtin pwd -L
    end
    function __fre_hook --on-variable PWD
      test -z "$fish_private_mode"
      and command fre --add (__fre_pwd)
    end
  '';
}
