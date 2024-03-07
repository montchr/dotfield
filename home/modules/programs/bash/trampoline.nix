{ lib, config, ... }:
let
  cfg = config.programs.bash.trampoline;
  inherit (lib) types;
in
{
  options.programs.bash.trampoline = {
    enable = lib.mkOption {
      default = false;
      description = lib.mdDoc ''
        Whether to load an alternate shell session during Bash initialisation.

        Allows alternate shells to inherit the environment from Bash,
        which is especially helpful when dealing with Nushell's different syntax.

        Requires that the user's default shell is set to bash.

        On Linux, nested sessions are not permitted in order to allow bash
        to run normally when invoked directly. This safeguard is currently
        not supported on Darwin due to BSD `ps` differences.

        <https://wiki.archlinux.org/title/Fish#Modify_.bashrc_to_drop_into_fish>
      '';
    };
    shell.package = lib.mkOption {
      type = with types; either bool package;
      default = false;
      description = lib.mdDoc ''
        Alternate shell package to replace the Bash session and inherit its environment.
      '';
    };
  };

  config = lib.mkIf cfg.enable {
    # FIXME: does not work on darwin -- args don't work w/darwin `ps` + $PPID not set
    # if [[ $(ps --no-header --pid=$PPID --format=comm) != "fish" && -z ''${BASH_EXECUTION_STRING} ]]
    programs.bash.initExtra = lib.mkAfter ''
      if [[ -z ''${BASH_EXECUTION_STRING} ]]
      then
      	shopt -q login_shell && LOGIN_OPTION='--login' || LOGIN_OPTION=""
        exec ${lib.getExe cfg.shell.package} $LOGIN_OPTION
      fi
    '';
  };
}
