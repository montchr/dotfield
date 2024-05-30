{ lib, pkgs, ... }:
{
  # TODO: what about rsa keys?
  sops.age.sshKeyPaths = [ "/etc/ssh/ssh_host_ed25519_key" ];

  home-manager.sharedModules = lib.singleton {
    # Allow normal users to use sops.
    home.sessionVariables = {
      "AGE_KEY_DIR" = lib.mkDefault "$HOME/.age";
      "SOPS_AGE_KEY_DIR" = lib.mkDefault "$XDG_CONFIG_HOME/sops/age";
      "SOPS_AGE_KEY_FILE" = lib.mkDefault "$XDG_CONFIG_HOME/sops/age/keys";
    };
  };

  environment.systemPackages = [
    pkgs.rage
    pkgs.sops
  ];
}
