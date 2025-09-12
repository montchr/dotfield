{
  aspects.core.nixos =
    {
      config,
      lib,
      pkgs,
      ...
    }:
    {
      sops.age.sshKeyPaths = [ "/etc/ssh/ssh_host_ed25519_key" ];
      users.groups.keys = { inherit (config.users.groups.wheel) members; };

      home-manager.sharedModules = lib.singleton {
        # Allow normal users to use sops.
        home.sessionVariables = {
          "AGE_KEY_DIR" = "$HOME/.age";
          "SOPS_AGE_KEY_DIR" = "$XDG_CONFIG_HOME/sops/age";
          "SOPS_AGE_KEY_FILE" = "$XDG_CONFIG_HOME/sops/age/keys";
        };
      };

      environment.systemPackages = [
        pkgs.rage
        pkgs.sops
      ];
    };
}
