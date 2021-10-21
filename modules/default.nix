{ config, pkgs, ... }:

{
  imports = [
    ./bash.nix
    ./bat.nix
    ./direnv.nix
    ./git.nix
    ./gpg.nix
    ./gui.nix
    ./node.nix
    ./python.nix
    ./ripgrep.nix
    ./settings.nix
    ./ssh.nix
    ./tealdeer.nix
    ./zsh.nix

    ./editors/emacs
    ./editors/vim
    ./espanso
    ./firefox
    ./kitty
    ./php
  ];

  my.modules = {
    bat.enable = true;
    bash.enable = true;
    direnv.enable = true;
    espanso.enable = true;
    firefox.enable = true;
    git.enable = true;
    gui.enable = true;
    kitty.enable = true;
    node.enable = true;
    python.enable = true;
    ripgrep.enable = true;
    ssh.enable = true;
    tealdeer.enable = true;
    zsh.enable = false;

    editors = {
      emacs.enable = true;
      emacs.doom.enable = true;
      vim.enable = true;
    };
  };

}
