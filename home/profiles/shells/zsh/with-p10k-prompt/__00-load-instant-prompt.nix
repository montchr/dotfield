{ config, ... }:
let
  inherit (config.xdg) cacheHome;
in
{
  programs.zsh.initContent = l.mkInitInstantPrompt ''
    emulate zsh -c "$(direnv export zsh)"

    # Enable Powerlevel10k instant prompt. Should stay close to the top of ~/.zshrc.
    # Initialization code that may require console input (password prompts, [y/n]
    # confirmations, etc.) must go above this block; everything else may go below.
    if [[ -r "${cacheHome}/p10k-instant-prompt-''${(%):-%n}.zsh" ]]; then
      source "${cacheHome}/p10k-instant-prompt-''${(%):-%n}.zsh"
    fi

    emulate zsh -c "$(direnv hook zsh)"
  '';
}
