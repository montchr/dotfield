{ config, pkgs, ... }:

{
  imports = [
    ./00-settings.nix
    ./01-shell.nix
    ./02-gpg.nix
    ./03-ssh.nix
    ./04-git.nix

    ./bat.nix
    ./gui.nix
    ./kitty.nix
    ./node.nix
    ./python.nix
    ./ripgrep.nix
    ./youtube-dl.nix
    # ./newsboat.nix
    # ./tmux.nix
    # ./vim.nix
  ];

  my.modules = {
    shell.enable = true;
    git.enable = true;
    gpg.enable = true;
    ssh.enable = true;

    # tmux.enable = true;
    # vim.enable = true;
    bat.enable = true;
    kitty.enable = true;
    misc.enable = true;
    node.enable = true;
    python.enable = true;
    ripgrep.enable = true;
  };

}
