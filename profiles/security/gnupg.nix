{
  config,
  lib,
  pkgs,
  ...
}: {
  programs.gnupg.agent = {
    enable = true;
    enableSSHSupport = true;
  };

  environment.variables = {
    GNUPGHOME = "$HOME/.local/share/gnupg";
  };

  environment.systemPackages = with pkgs; [
    gnupg
    gpgme

    (
      if pkgs.stdenv.isDarwin
      then pkgs.pinentry_mac
      else pkgs.pinentry
    )

    (writeShellScriptBin "gpg-agent-restart" ''
      pkill gpg-agent ; pkill ssh-agent ; pkill pinentry ; eval $(gpg-agent --daemon --enable-ssh-support)
    '')
  ];
}
