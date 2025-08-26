{ lib, ... }:
{
  dotfield.baseline.home =
    { config, ... }:
    {
      programs.ssh = {
        enable = true;
        matchBlocks = {
          "github.com" = {
            user = "git";
          };

          "eu.nixbuild.net" = {
            extraOptions = {
              PubkeyAcceptedKeyTypes = "ssh-ed25519";
            };
          };

          "*" = {
            identityFile = lib.mkDefault "${config.home.homeDirectory}/.ssh/id_ed25519";
            identitiesOnly = true;
            forwardAgent = false;
            serverAliveInterval = 300;
            controlMaster = "auto";
            controlPersist = "10m";
          };
        };
      };
    };
}
