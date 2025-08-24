flake@{ lib, inputs, ... }:
let
  inherit (inputs.apparat.lib.net.constants) dns;
in
{
  dotfield.baseline.nixos = {
    i18n.defaultLocale = "en_US.UTF-8";

    services.dbus.implementation = "broker";

    documentation.nixos.enable = false;

    programs.fish.enable = true;

    networking.nameservers = dns.nameservers.quad9;
    networking.networkmanager.insertNameservers = dns.nameservers.quad9;

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
      hostKeys = [
        {
          bits = 4096;
          path = "/etc/ssh/ssh_host_rsa_key";
          type = "rsa";
        }
        {
          path = "/etc/ssh/ssh_host_ed25519_key";
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

    # Prevent root login-by-password.
    users.users.root.password = "";
    users.users.root.openssh.authorizedKeys.keys = flake.config.dotfield.meta.users.cdom.keys.ssh;
    services.openssh.settings.PermitRootLogin = lib.mkDefault "prohibit-password";

    hardware.enableRedistributableFirmware = true;
  };

  dotfield.baseline.home =
    { config, pkgs, ... }:
    let
      # Although it points to a commonly-used path for user-owned executables,
      # $XDG_BIN_HOME is a non-standard environment variable. It is not part of
      # the XDG Base Directory Specification.
      #
      # https://specifications.freedesktop.org/basedir-spec/basedir-spec-latest.html
      #
      # NOTE: This may also be set at the system level -- it is included again here
      # for standalone installation parity.
      binHome = "$HOME/.local/bin";
    in

    {
      xdg.enable = true;

      programs.bash.enable = true;
      programs.jq.enable = true;
      programs.man.enable = true;

      programs.nix-index.enable = true;
      # FIXME: does not exist
      # programs.nix-index.symlinkToCacheHome = true;
      # FIXME: does not exist
      # programs.nix-index-database.comma.enable = true;

      # User-defined executables should always be prioritized in $PATH.
      home.sessionPath = lib.mkBefore [ binHome ];

      home.sessionVariables = {
        "EDITOR" = lib.mkDefault "vim";
        "LESSHISTFILE" = "${config.xdg.stateHome}/lesshst";
        "XDG_BIN_HOME" = binHome;
      };

    };
}
