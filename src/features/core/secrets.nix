{ lib, inputs, ... }:
{
  dotfield.nixos =
    { config, pkgs, ... }:
    lib.mkMerge [
      {
        imports = [ inputs.sops-nix.nixosModules.sops ];

        sops.age.sshKeyPaths = [ "/etc/ssh/ssh_host_ed25519_key" ];

        environment.systemPackages = [
          pkgs.rage
          pkgs.sops
        ];
      }
    ]
    ++ (config.lib.generateSudoersExtraGroupsModules [ "keys" ]);

  dotfield.home = {
    # Allow normal users to use sops.
    home.sessionVariables = {
      "AGE_KEY_DIR" = "$HOME/.age";
      "SOPS_AGE_KEY_DIR" = "$XDG_CONFIG_HOME/sops/age";
      "SOPS_AGE_KEY_FILE" = "$XDG_CONFIG_HOME/sops/age/keys";
    };
  };
}
