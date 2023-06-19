{
  config,
  lib,
  pkgs,
  ops,
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

  networking.nameservers = lib.mkDefault ops.metadata.dns.ns.quad9;

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
    # For age encryption, all hosts need a ssh key pair.
    enable = true;
    # If, for some reason, a machine should not be accessible by SSH,
    # then restrict access through firewall rather than disabling SSH entirely.
    openFirewall = lib.mkDefault true;
    settings.PasswordAuthentication = false;
    settings.PermitRootLogin = lib.mkDefault "prohibit-password";
    hostKeys = [
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
  };

  # Passwordless sudo when SSH'ing with keys
  security.pam.enableSSHAgentAuth = true;

  # FIXME: too open!!! set per-host explicitly.
  users.users.root.openssh.authorizedKeys.keys = ops.users.cdom.keys.default;

  hardware.enableRedistributableFirmware = lib.mkDefault true;
}
