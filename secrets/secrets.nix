let
  machines = {
    boschic = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIJxqOfaPK3FQnBsp7ey3yUEuFnYzAJSF1PdyJLqpKAfK";
    hierophant = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIF14I8s1ywGVCmInco+wfCG5C9kJB0Y9HCOA12ZX9cHe";
    HodgePodge = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIPRS3UkaJeMQm58v+ggR5e0hVeUbFZkhyQJwEC0LK5YS";
    parrothelles = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAINBvBCsqtgEdC4J+d1xzrwPIircRYSKbFHR0FulaNV5z";
    tso-portal = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAINUnCW5QopDKLISa/kRcH+28n9QUV/nFuYadXqUp/ZVq";
    tapestone = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAICdk0EzvFodiLIxxheOWQTAav92uZwfMVR1k/0TS7yal";
  };

  trustedUsers = import ./authorized-keys.nix;

  servers = with machines; [
    hierophant
    tapestone
    tso-portal
  ];
  workstations = with machines; [
    boschic
    HodgePodge
    parrothelles
  ];
  allMachines = servers ++ workstations;
in {
  "wireless.env.age".publicKeys = allMachines ++ trustedUsers;

  "aws/aws-cdom-default.pem.age".publicKeys = allMachines ++ trustedUsers;

  "espanso/personal.yml.age".publicKeys = allMachines ++ trustedUsers;
  "espanso/work.yml.age".publicKeys = trustedUsers;
}
