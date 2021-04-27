{ config, pkgs, ... }:

{
  imports = [
    ./xdg.nix
    ./settings.nix
    ./shell.nix
    ./direnv.nix
    # ./mail.nix
    ./gpg.nix
    ./ssh.nix
    ./git.nix
    ./kitty.nix
    ./bat.nix
    # ./lf.nix
    # ./mpv.nix
    # ./newsboat.nix
    ./ripgrep.nix
    # ./tmux.nix
    # ./youtube-dl.nix
    # ./misc.nix
    # ./vim.nix
    ./node.nix
    # ./deno.nix
    # ./irc.nix
    # ./go.nix
    # ./rescript.nix
    ./gui.nix
    ./python.nix
    # ./syncthing.nix
  ];

  my.modules = {
    shell.enable = true;
    git.enable = true;
    ssh.enable = true;
    direnv.enable = true;
    # syncthing.enable = true;

    kitty.enable = true;
    bat.enable = true;
    # lf.enable = true;
    # mpv.enable = true;
    python.enable = true;
    ripgrep.enable = true;
    # tmux.enable = true;
    # misc.enable = true;
    # vim.enable = true;
    gui.enable = true;

    node.enable = true;
    # deno.enable = true;
    # go.enable = true;
    # rust.enable = false;
  };

}
