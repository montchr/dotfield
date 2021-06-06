{ pkgs, lib, config, ... }:

let
  cfg = config.my.modules.ssh;
in {
  options = with lib; {
    my.modules.ssh = {
      enable = mkEnableOption ''
        Whether to enable ssh module
      '';
    };
  };

  config = with lib;
    mkIf cfg.enable {
      my.hm.file = {
        ".ssh/config" = {
          text = ''
            # https://github.com/drduh/config/blob/master/ssh_config
            # https://linux.die.net/man/5/ssh_config

            Include ${config.my.user.home}/.config/ssh/config.local

            #Host router
            #  IdentityFile ~/.ssh/router
            #  HostName 192.168.1.1
            #  Port 2222
            #  User sysadm
            #  ControlMaster auto
            #  ControlPath ~/.ssh/master-%r@%h:%p
            #  ControlPersist 300

            Host github.com
              User git
              ControlMaster no
              IdentitiesOnly yes
              IdentityFile ~/.ssh/id_ed25519_yubikey.pub
              MACs hmac-sha2-512-etm@openssh.com,hmac-sha2-256-etm@openssh.com,umac-128-etm@openssh.com,hmac-sha2-512,hmac-sha2-256,umac-128@openssh.com

            Host *
              #ControlMaster auto
              #ControlPath ~/.ssh/master-%r@%h:%p
              #ControlPersist 300
              AddKeysToAgent yes
              AddressFamily inet
              IdentityFile ~/.ssh/id_ed25519_yubikey.pub
              HashKnownHosts yes
              VisualHostKey yes
              PasswordAuthentication no
              ChallengeResponseAuthentication no
              StrictHostKeyChecking ask
              VerifyHostKeyDNS yes
              ForwardAgent no
              ForwardX11 no
              ForwardX11Trusted no
              ServerAliveInterval 300
              ServerAliveCountMax 2
              Ciphers chacha20-poly1305@openssh.com,aes256-gcm@openssh.com
              MACs hmac-sha2-512-etm@openssh.com,hmac-sha2-256-etm@openssh.com
              KexAlgorithms curve25519-sha256@libssh.org,diffie-hellman-group-exchange-sha256
              HostKeyAlgorithms ssh-ed25519-cert-v01@openssh.com,ssh-rsa-cert-v01@openssh.com,ssh-ed25519,ssh-rsa
          '';
        };
      };
    };
}
