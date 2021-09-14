{
  imports = [
    ./bash.nix
    ./bat.nix
    ./direnv.nix
    ./git.nix
    ./gpg.nix
    ./gui.nix
    ./lorri.nix
    ./node.nix
    ./php.nix
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
    ./secrets
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
    lorri.enable = true;
    node.enable = true;
    # FIXME: disabled due to build failures -- i haven't actually taken the time to configure this module
    python.enable = false;
    ripgrep.enable = true;
    secrets.enable = true;
    ssh.enable = true;
    tealdeer.enable = true;
    zsh.enable = true;

    editors = {
      emacs.enable = true;
      emacs.doom.enable = true;
      vim.enable = true;
    };
  };
}
