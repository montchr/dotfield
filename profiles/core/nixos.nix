moduleArgs @ {
  self,
  config,
  lib,
  pkgs,
  ...
}: let
  sshHostPath =
    if (moduleArgs.impermanence or false)
    then "/persist/etc/ssh"
    else "/etc/ssh";
in {
  nix = {
    autoOptimiseStore = true;
    nixPath = ["nixos-config=${../../lib/compat/nixos}"];
    optimise.automatic = true;
    systemFeatures = ["nixos-test" "benchmark" "big-parallel" "kvm"];
  };

  boot.loader.systemd-boot.consoleMode = "auto";
  boot.cleanTmpDir = lib.mkDefault true;

  environment.shellAliases = {
    # Fix `nixos-option` for flake compatibility
    nixos-option = "nixos-option -I nixpkgs=${self}/lib/compat";
  };

  environment.systemPackages = with pkgs; [
    dosfstools
    efibootmgr
    gptfdisk
    inetutils
    iputils
    mtr
    pciutils
    sysstat
    usbutils
    utillinux
  ];

  programs.git.enable = true;
  programs.git.config = {
    safe.directory = [
      "/etc/nixos"
      "/etc/dotfield"
    ];
  };

  security.sudo.wheelNeedsPassword = false;

  services.openssh = {
    # For rage encryption, all hosts need a ssh key pair
    enable = lib.mkForce true;

    openFirewall = true;
    passwordAuthentication = false;
    permitRootLogin = "prohibit-password";
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
