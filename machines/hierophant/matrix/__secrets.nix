{ config, ... }:
let
  inherit (config.users) users;
in
{
  sops.secrets."matrix-synapse/client-secret-yaml" = {
    owner = users.matrix-synapse.name;
    group = users.matrix-synapse.group;
    mode = "0440";
    restartUnits = [ "matrix-synapse.service" ];
  };

  sops.secrets."matrix-synapse/email-config-yaml" = {
    owner = users.matrix-synapse.name;
    group = users.matrix-synapse.group;
    mode = "0440";
    restartUnits = [ "matrix-synapse.service" ];
  };

  sops.secrets."matrix-synapse/recaptcha-private-key-yaml" = {
    owner = users.matrix-synapse.name;
    group = users.matrix-synapse.group;
    mode = "0440";
    restartUnits = [ "matrix-synapse.service" ];
  };

  sops.secrets."matrix-synapse/registration-shared-secret-yaml" = {
    owner = users.matrix-synapse.name;
    group = users.matrix-synapse.group;
    mode = "0440";
    restartUnits = [ "matrix-synapse.service" ];
  };

  users.users.matrix-synapse.extraGroups = [ config.users.groups.keys.name ];
  systemd.services.matrix-synapse.serviceConfig.SupplementaryGroups = [
    config.users.groups.keys.name
  ];
}
