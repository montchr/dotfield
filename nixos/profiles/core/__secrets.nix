{
  config,
  lib,
  pkgs,
  ...
}:
let
  inherit (config.dotfield.paths) storageBase;
  sshPath = "${storageBase}/etc/ssh";
in
{
  # TODO: what about rsa keys?
  sops.age.sshKeyPaths = [ "${sshPath}/ssh_host_ed25519_key" ];

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
}
