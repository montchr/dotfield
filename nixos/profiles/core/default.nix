{
  config,
  lib,
  ops,
  pkgs,
  flake,
  ...
}:
let
  inherit (flake.inputs.apparat.constants.networking) dns;
  # FIXME: idk, i don't like the idea that this well-known directory
  #        would be located at `/persist/etc/ssh/` instead of `/etc/ssh/`...
  inherit (config.dotfield.paths) storageBase;
  sshHostPath = "${storageBase}/etc/ssh";
in
{
  imports = [
    ./__environment.nix
    ./__home-manager.nix
    ./__nh.nix
    ./__nix-index.nix
    ./__nix-config.nix
    ./__secrets.nix
    ./__system-packages.nix
  ];

  # The only sane default. Servers should usually keep this as is.
  time.timeZone = lib.mkDefault "UTC";

  programs.fish.enable = true;

  programs.zsh = {
    enable = true;
    shellInit = "";
    loginShellInit = "";
    interactiveShellInit = "";

    # Prompts/completions/widgets should never be initialised at the
    # system-level because it will need to be initialised a second time once the
    # user's zsh configs load.
    enableCompletion = lib.mkForce false;
    enableBashCompletion = lib.mkForce false;
    promptInit = lib.mkForce "";
    syntaxHighlighting.enable = lib.mkForce false;
  };

  networking.nameservers = lib.mkDefault dns.nameservers.cloudflare;

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
    openFirewall = true;
    settings.PasswordAuthentication = false;
    settings.PermitRootLogin = "prohibit-password";
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
  # NOTE: Specifying user-writeable files will result in an insecure
  #       configuration.
  #       A malicious process can then edit such an authorized_keys file and
  #       bypass the ssh-agent-based authentication.  See NixOS/nixpkgs#31611
  security.pam.sshAgentAuth = {
    enable = true;
    # Matches new default from nixpkgs#31611
    authorizedKeysFiles = [ "/etc/ssh/authorized_keys.d/%u" ];
  };

  # TODO: reduce number of keys with access
  users.users.root.openssh.authorizedKeys.keys = ops.users.cdom.keys.default;

  hardware.enableRedistributableFirmware = true;
}
