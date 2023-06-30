{
  config,
  lib,
  pkgs,
  ...
}: let
  inherit (config.dotfield.paths) storageBase;
  sshPath = "${storageBase}/etc/ssh";
in {
  home-manager.sharedModules = [
    {
      # Allow running sops as a normal user without sudo.
      home.sessionVariables = {
        "SOPS_AGE_KEY_DIR" = lib.mkDefault "$XDG_CONFIG_HOME/sops/age";
        "SOPS_AGE_KEY_FILE" = lib.mkDefault "$XDG_CONFIG_HOME/sops/age/keys";
      };
    }
  ];

  environment.systemPackages = [pkgs.sops];

  sops.age.sshKeyPaths = ["${sshPath}/ssh_host_ed25519_key"];

  # This can be overridden per-host for localised secrets.
  sops.defaultSopsFile = lib.mkDefault ../secrets/global.secrets.yaml;
}
