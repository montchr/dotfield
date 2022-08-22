{
  config,
  lib,
  pkgs,
  ...
}: {
  services.pcscd.enable = true;
  services.udev.packages = with pkgs; [yubikey-personalization];

  # Use GnuPG's agent for SSH auth.
  programs.ssh.startAgent = false;
  programs.gnupg.agent = {
    enable = true;
    enableSSHSupport = true;
  };
}
