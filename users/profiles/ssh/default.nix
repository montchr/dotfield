{ pkgs, lib, config, ... }:

let
  identityFile = "~/.ssh/id_ed25519_yubikey.pub";
in

{
  # Ensure correct permissions
  system.activationScripts.postUserActivation.text = ''
    chmod 600 ${identityFile}
  '';

  my.hm.programs.ssh = {
    enable = true;
    hashKnownHosts = true;
    forwardAgent = false;

    matchBlocks = {

      "github.com" = {
        user = "git";
        identitiesOnly = true;
        extraOptions = {
          "MACs" = "hmac-sha2-512-etm@openssh.com,hmac-sha2-256-etm@openssh.com,umac-128-etm@openssh.com,hmac-sha2-512,hmac-sha2-256,umac-128@openssh.com";
        };
      };

      "*" = {
        forwardX11 = false;
        forwardX11Trusted = false;
        serverAliveInterval = 300;
        serverAliveCountMax = 2;
      };

    };

    extraConfig = ''
      Include ${config.my.user.home}/.config/ssh/config.local

      AddKeysToAgent yes
      AddressFamily inet
      ChallengeResponseAuthentication no
      IdentityFile ${identityFile}
      PasswordAuthentication no
      StrictHostKeyChecking ask
      VerifyHostKeyDNS yes

      Ciphers chacha20-poly1305@openssh.com,aes256-gcm@openssh.com
      HostKeyAlgorithms ssh-ed25519-cert-v01@openssh.com,ssh-rsa-cert-v01@openssh.com,ssh-ed25519,ssh-rsa
      KexAlgorithms curve25519-sha256@libssh.org,diffie-hellman-group-exchange-sha256
      MACs hmac-sha2-512-etm@openssh.com,hmac-sha2-256-etm@openssh.com
    '';
  };
}
