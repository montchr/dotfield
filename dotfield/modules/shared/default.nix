{ config, pkgs, ... }:

{
  imports = [
    ./xdg.nix
    ./settings.nix
    ./shell.nix
    ./direnv.nix
    ./gpg.nix
    ./ssh.nix
    ./git.nix
    ./kitty.nix
    ./bat.nix
    ./ripgrep.nix
    ./node.nix
    ./gui.nix
    ./python.nix
  ];

  my.modules = {
    bat.enable = true;
    direnv.enable = true;
    git.enable = true;
    gui.enable = true;
    kitty.enable = true;
    node.enable = true;
    python.enable = true;
    ripgrep.enable = true;
    shell.enable = true;
    ssh.enable = true;
  };

}
