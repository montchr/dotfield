flake@{ ... }:
{
  aspects.core = {
    nixos = {
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

      # TODO: reduce number of keys with access
      users.users.root.openssh.authorizedKeys.keys = flake.config.meta.users.cdom.keys.ssh;
    };
  };
}
