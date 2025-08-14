{ lib, moduleWithSystem, ... }:
{
  dotfield.features."bash/with-fzf-tab-completion".home = moduleWithSystem (
    perSystem@{ config, ... }:
    {
      programs.bash.initExtra = lib.mkAfter ''
        source ${perSystem.config.packages.fzf-tab-completion}/share/bash/fzf-bash-completion.sh
        bind -x '"\t": fzf_bash_completion'
      '';
    }
  );
}
