let
  machines = {
    alleymon = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIJsVn0I6Q0rL94W2V89efhUiffAeJfDtHYcW6czXcPkh";
    boschic = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIJxqOfaPK3FQnBsp7ey3yUEuFnYzAJSF1PdyJLqpKAfK";
    hierophant = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIF14I8s1ywGVCmInco+wfCG5C9kJB0Y9HCOA12ZX9cHe";
    HodgePodge = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIPRS3UkaJeMQm58v+ggR5e0hVeUbFZkhyQJwEC0LK5YS";
    parrothelles = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAINBvBCsqtgEdC4J+d1xzrwPIircRYSKbFHR0FulaNV5z";
    tso-portal = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAINUnCW5QopDKLISa/kRcH+28n9QUV/nFuYadXqUp/ZVq";
    tapestone = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAICdk0EzvFodiLIxxheOWQTAav92uZwfMVR1k/0TS7yal";
  };

  trustedUsers = import ./authorized-keys.nix;

  alleymonRsa = "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABgQDg+L0ma2MoSDA9c+ydaOn+is1UWaMvTJJSdEbTsJ0qn75JyNaVB3c2nIqbu3kTS18z6WDeGkAuMHVwJLZWiL+JKqQuAgvLm3pSH7Wmtfako5T3+3xcw6DKgteVFQC0819Xh4CaOCirDSHBdUk/dqZEi5HuYHxmLmd39crVqAV60csS1LXFNgmPF4QvLj0NUcXJyHEkfSgRHKWwZ2JywN5pURc6/7Vl7b/j+r2nvsWcP4yqtAYjSgnxO+H7QZWZ040eMFNqgcqP+LclI3l0jEMW/SC8ZlqvS9hiICXJ6YLQCZmbRfnFxO1IxZwu97892pV/9ZsdFEiLq/fT2f8rTVZLYe4xLIfSw7BNlV7K9VDyD6PLQ9blKlioser2UjTxB/FH1NdoD7YBDhcMiYg9CfVBM34Q98mB21OwysHHLF7ukZ4Mk5H64y2cuCWRG+ze4CPLu7gK0zTb32eZh9AmUqljZgPs3FL28BJAWwZii4jzV1Hd7KTZ25Pshrk1bCpfAI8=";

  servers = with machines; [
    hierophant
    tapestone
    tso-portal
  ];
  workstations = with machines; [
    alleymon
    boschic
    HodgePodge
    parrothelles
  ];
  allMachines = servers ++ workstations;
in {
  "wireless.env.age".publicKeys = allMachines ++ trustedUsers;

  "aws/aws-cdom-default.pem.age".publicKeys = allMachines ++ trustedUsers;

  "espanso/personal.yml.age".publicKeys = allMachines ++ trustedUsers;
  "espanso/work.yml.age".publicKeys = [machines.alleymon] ++ trustedUsers;
}
