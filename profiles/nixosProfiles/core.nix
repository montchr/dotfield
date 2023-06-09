{
  config,
  lib,
  pkgs,
  ...
}: let
  inherit (config.dotfield.paths) storageBase;
  sshHostPath = "${storageBase}/etc/ssh";
in {
  # If this were enabled, rebuilds will take... a very long time.
  documentation.info.enable = false;

  nix = {
    settings = {
      system-features = ["nixos-test" "benchmark" "big-parallel" "kvm"];
    };
    gc.dates = "weekly";
    optimise.automatic = true;
  };

  environment.systemPackages = with pkgs; [
    dosfstools
    gptfdisk
    inetutils
    iputils
    pciutils
    sysstat
    usbutils
    util-linux
  ];

  users.defaultUserShell = pkgs.fish;
  programs.zsh.syntaxHighlighting.enable = lib.mkForce false;

  programs.git.enable = true;
  programs.git.config = {
    safe.directory = [
      "/etc/nixos"
      "/etc/dotfield"
    ];
  };

  programs.htop.enable = true;
  programs.mtr.enable = true;

  services.openssh = {
    # For rage encryption, all hosts need a ssh key pair
    enable = lib.mkForce true;
    openFirewall = true;
    settings.passwordAuthentication = false;
    settings.permitRootLogin = "prohibit-password";
  };

  services.openssh.hostKeys = [
    {
      bits = 4096;
      path = "${sshHostPath}/ssh_host_rsa_key";
      type = "rsa";
    }
    {
      path = "${sshHostPath}/ssh_host_ed25519_key";
      type = "ed25519";
    }
  ];

  # Allow passwordless sudo within an SSH session.
  security.pam.enableSSHAgentAuth = true;

  hardware.enableRedistributableFirmware = lib.mkDefault true;
}