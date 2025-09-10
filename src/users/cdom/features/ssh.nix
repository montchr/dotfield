flake@{ ... }:
{
  users.cdom.aspects.core.home =
    { config, ... }:
    let
      inherit (config.home) homeDirectory;
      inherit (flake.config.meta) hosts;

      sshDir = "${homeDirectory}/.ssh";
      identityFile = "${sshDir}/id_ed25519";
    in
    {

      programs.ssh = {
        enable = true;
        enableDefaultConfig = false;
        matchBlocks = {
          "synoxyn" = {
            hostname = hosts.synoxyn.ipv4.address;
            port = 2367;
          };

          "atlantis" = {
            hostname = "atlantis.whatbox.ca";
            user = "syadasti";
          };

          "eu.nixbuild.net" = {
            identityFile = "${sshDir}/id_ed25519_seadome_nixbuild_net";
          };

          "*" = {
            inherit identityFile;

            addKeysToAgent = "no";
            compression = false;
            controlMaster = "auto";
            controlPath = "~/.ssh/master-%r@%n:%p";
            controlPersist = "10m";
            forwardAgent = false;
            hashKnownHosts = false;
            identitiesOnly = true;
            serverAliveCountMax = 3;
            serverAliveInterval = 300;
            userKnownHostsFile = "~/.ssh/known_hosts";
          };
        };
      };
    };
}
