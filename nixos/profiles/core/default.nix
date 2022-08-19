moduleArgs @ {
  self,
  config,
  lib,
  pkgs,
  profiles,
  ...
}: let
  inherit (config.lib.dotfield) fsPath;

  sshHostPath =
    if (moduleArgs.impermanence or false)
    then "/persist/etc/ssh"
    else "/etc/ssh";

  # FIXME: is this accurate?
  nixosConfigPath = "${fsPath}/lib/compat/nixos";
in {
  imports = with (profiles.common); [core];

  nix = {
    settings = {
      auto-optimise-store = true;
      # TODO: is it really reasonable to set these all as defaults?
      system-features = ["nixos-test" "benchmark" "big-parallel" "kvm"];
    };

    nixPath = ["nixos-config=${nixosConfigPath}"];
    optimise.automatic = true;
  };

  boot.loader.systemd-boot.consoleMode = "auto";
  boot.cleanTmpDir = lib.mkDefault true;

  environment.shellAliases = {
    # Fix `nixos-option` for flake compatibility
    # FIXME: it's broken
    # nixos-option = "nixos-option -I nixpkgs=${self}/lib/compat";
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
