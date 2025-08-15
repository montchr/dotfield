flake@{ lib, ... }:
{
  dotfield.features.installer.nixos = {
    imports = [
      flake.config.dotfield.features.remote-builders.nixos
    ];

    users.users.nixos = {
      password = "nixos";
      description = "default";
      isNormalUser = true;
      extraGroups = [ "wheel" ];
    };

    services.openssh.settings.PermitRootLogin = lib.mkForce "yes";
  };
}
