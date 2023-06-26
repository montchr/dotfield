{
  lib,
  config,
  ...
}: {
  # Initialise fish from a bash shell to inherit its environment.
  #
  # Requires that the user's default shell is set to bash.
  #
  # fish will not be re-initialised if the parent process is also fish,
  # allowing bash to run normally when invoked directly.
  #
  # <https://wiki.archlinux.org/title/Fish#Modify_.bashrc_to_drop_into_fish>
  programs.bash.initExtra = lib.mkAfter ''
    if [[ $(ps --no-header --pid=$PPID --format=comm) != "fish" && -z ''${BASH_EXECUTION_STRING} ]]
    then
    	shopt -q login_shell && LOGIN_OPTION='--login' || LOGIN_OPTION=""
      exec ${config.programs.fish.package}/bin/fish $LOGIN_OPTION
    fi
  '';
}
