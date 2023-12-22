{
  config,
  lib,
  pkgs,
  ops,
  flake,
  ...
}: let
  inherit (flake.inputs.apparat.constants.networking) dns;
  # FIXME: idk, i don't like the idea that this well-known directory
  #        would be located at `/persist/etc/ssh/` instead of `/etc/ssh/`...
  inherit (config.dotfield.paths) storageBase;
  sshHostPath = "${storageBase}/etc/ssh";
in {
  imports = [
    ./nix-config.nix
    ./system-tools.nix
  ];

  networking.nameservers = lib.mkDefault dns.nameservers.cloudflare;

  # TODO: why forced?
  programs.zsh.syntaxHighlighting.enable = lib.mkForce false;

  programs.git.enable = true;
  programs.git.config = {
    safe.directory = [
      # owner should be root:wheel but i cheat and make my own user the owner
      # in some ideal situation, maybe i would deploy the configuration from $XDG_CONFIG_DIR/dotfield
      "/etc/nixos"
      "/etc/dotfield"
    ];
  };

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

  # TODO: reduce number of keys with access
  users.users.root.openssh.authorizedKeys.keys = ops.users.cdom.keys.default;

  hardware.enableRedistributableFirmware = lib.mkDefault true;
}
