{ self, ... }:
{
  aspects.core.home =
    { config, ... }:
    {
      imports = [
        self.modules.homeManager.bash-trampoline
      ];

      home.extraOutputsToInstall = [ "/share/bash-completion" ];

      programs.bash = {
        enable = true;
        enableCompletion = true;
        sessionVariables = {
          "BASH_COMPLETION_USER_FILE" = "${config.xdg.dataHome}/bash/completion";
        };
      };
    };
}
