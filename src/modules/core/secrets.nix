{ inputs, ... }:
{
  dotfield.modules.hosts.nixos =
    { pkgs, ... }:
    {
      imports = [ inputs.sops-nix.nixosModules.sops ];

      sops.age.sshKeyPaths = [ "/etc/ssh/ssh_host_ed25519_key" ];

      environment.systemPackages = [
        pkgs.rage
        pkgs.sops
      ];
    };

  dotfield.home.defaults = {
    # Allow normal users to use sops.
    home.sessionVariables = {
      "AGE_KEY_DIR" = "$HOME/.age";
      "SOPS_AGE_KEY_DIR" = "$XDG_CONFIG_HOME/sops/age";
      "SOPS_AGE_KEY_FILE" = "$XDG_CONFIG_HOME/sops/age/keys";
    };
  };
}
