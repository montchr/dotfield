moduleArgs @ {
  config,
  lib,
  pkgs,
  peers,
  ...
}: let
  inherit (peers.hosts) hierophant tsone;
  identityFileName = "id_rsa_yk.pub";
  identityFile = "~/.ssh/${identityFileName}";
in {
  home.file.".ssh/${identityFileName}".source = ../../identity/ssh-yubikey.pub;

  programs.ssh = {
    enable = true;
    forwardAgent = false;
    serverAliveInterval = 300;
    serverAliveCountMax = 2;

    includes = ["~/.config/ssh/config.local"];

    matchBlocks = {
      "tsone" = {
        host = tsone.ipv4.address;
        user = "cdom";
      };

      "hierophant" = {
        host = hierophant.ipv4.address;
      };

      "github.com" = {
        # inherit identityFile;
        # identitiesOnly = true;
        user = "git";
        extraOptions = {
          "MACs" = "hmac-sha2-512-etm@openssh.com,hmac-sha2-256-etm@openssh.com,umac-128-etm@openssh.com,hmac-sha2-512,hmac-sha2-256,umac-128@openssh.com";
        };
        extraOptions = {
          ControlMaster = "no";
        };
      };

      "*" = {
        addressFamily = "inet";
        forwardX11 = false;
        forwardX11Trusted = false;
        serverAliveInterval = 300;
        serverAliveCountMax = 2;

        extraOptions = {
          AddKeysToAgent = "yes";
          ChallengeResponseAuthentication = "no";
          PasswordAuthentication = "no";
          StrictHostKeyChecking = "ask";
          VerifyHostKeyDNS = "yes";
          VisualHostKey = "yes";

          Ciphers = "chacha20-poly1305@openssh.com,aes256-gcm@openssh.com";
          HostKeyAlgorithms = "ssh-ed25519-cert-v01@openssh.com,ssh-rsa-cert-v01@openssh.com,ssh-ed25519,ssh-rsa";
          KexAlgorithms = "curve25519-sha256@libssh.org,diffie-hellman-group-exchange-sha256";
          MACs = "hmac-sha2-512-etm@openssh.com,hmac-sha2-256-etm@openssh.com";
        };
      };
    };
  };
}
## References:
# https://github.com/drduh/config/blob/master/ssh_config
# https://linux.die.net/man/5/ssh_config

