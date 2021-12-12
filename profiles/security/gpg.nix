{ config, pkgs, ... }:
{
  environment.systemPackages = with pkgs; [
    gnupg
    pinentry-curses
    pinentry-qt
    paperkey
    wget
  ];

  programs.ssh.startAgent = false;
  programs.gnupg.agent = {
    enable = true;
    enableSSHSupport = true;
  };

  services.udev.packages = with pkgs; [ yubikey-personalization ];
  services.pcscd.enable = true;

  environment.shellInit = ''
    export GPG_TTY="$(tty)"
    gpg-connect-agent /bye
    export SSH_AUTH_SOCK="/run/user/$UID/gnupg/s.gpg-agent.ssh"
  '';
}
