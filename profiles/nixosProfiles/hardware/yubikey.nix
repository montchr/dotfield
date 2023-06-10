{pkgs, ...}: {
  services.pcscd.enable = true;
  services.udev.packages = with pkgs; [yubikey-personalization];

  # TODO: move to another profile -- is not necessarily linked to yubikey
  # Use GnuPG's agent for SSH auth.
  # programs.ssh.startAgent = false;
  # programs.gnupg.agent = {
  #   enable = true;
  #   enableSSHSupport = true;
  # };
}
