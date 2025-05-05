# <https://github.com/romkatv/powerlevel10k#how-do-i-initialize-direnv-when-using-instant-prompt>
{
  imports = [
    ../_custom-prompt.nix

    ./__00-load-instant-prompt.nix
    ./__99-load-prompt-config.nix
  ];

  programs.direnv.enableZshIntegration = false;
}
