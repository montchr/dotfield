# <https://github.com/romkatv/powerlevel10k#how-do-i-initialize-direnv-when-using-instant-prompt>
{
  config,
  lib,
  ...
}: let
  inherit (config.xdg) cacheHome;
  dotfieldDir = config.home.sessionVariables."DOTFIELD_DIR";
  DOTFIELD_USER_ZDOTDIR = "${dotfieldDir}/users/cdom/config/zsh";
  l = import ./lib.nix {inherit lib;};
in {
  imports = [./custom-prompt.nix];

  programs.direnv.enableZshIntegration = false;

  programs.zsh.initExtraFirst = l.mkInitInstantPrompt ''
    emulate zsh -c "$(direnv export zsh)"

    # Enable Powerlevel10k instant prompt. Should stay close to the top of ~/.zshrc.
    # Initialization code that may require console input (password prompts, [y/n]
    # confirmations, etc.) must go above this block; everything else may go below.
    if [[ -r "${cacheHome}/p10k-instant-prompt-''${(%):-%n}.zsh" ]]; then
      source "${cacheHome}/p10k-instant-prompt-''${(%):-%n}.zsh"
    fi

    emulate zsh -c "$(direnv hook zsh)"
  '';

  programs.zsh.initExtra = l.mkInitPrompt ''
    [[ -f ${DOTFIELD_USER_ZDOTDIR}/.p10k.zsh ]] \
      && source ${DOTFIELD_USER_ZDOTDIR}/.p10k.zsh
  '';
}
