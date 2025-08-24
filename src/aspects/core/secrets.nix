{ inputs, ... }:
{
  dotfield.baseline.nixos =
    { config, pkgs, ... }:
    {
      imports = [ inputs.sops-nix.nixosModules.sops ];

      sops.age.sshKeyPaths = [ "/etc/ssh/ssh_host_ed25519_key" ];

      environment.systemPackages = [
        pkgs.rage
        pkgs.sops
      ];

      users.groups.keys.members = config.users.groups.wheel.members;
    };

  dotfield.baseline.home = {
    # Allow normal users to use sops.
    home.sessionVariables = {
      "AGE_KEY_DIR" = "$HOME/.age";
      "SOPS_AGE_KEY_DIR" = "$XDG_CONFIG_HOME/sops/age";
      "SOPS_AGE_KEY_FILE" = "$XDG_CONFIG_HOME/sops/age/keys";
    };
  };
}
