{
  flake,
  config,
  lib,
  pkgs,
  ...
}:
let
  inherit (flake.inputs) sops-nix;
  inherit (config.dotfield.paths) storageBase;
  sshPath = "${storageBase}/etc/ssh";
in
{
  imports = [ sops-nix.nixosModules.sops ];

  home-manager.sharedModules = lib.singleton {
    # Allow running sops as a normal user without sudo.
    home.sessionVariables = {
      "AGE_KEY_DIR" = lib.mkDefault "$HOME/.age";
      "SOPS_AGE_KEY_DIR" = lib.mkDefault "$XDG_CONFIG_HOME/sops/age";
      "SOPS_AGE_KEY_FILE" = lib.mkDefault "$XDG_CONFIG_HOME/sops/age/keys";
    };
  };

  environment.systemPackages = [
    pkgs.age-plugin-yubikey
    pkgs.rage
    pkgs.sops
  ];

  sops.age.sshKeyPaths = [ "${sshPath}/ssh_host_ed25519_key" ];
}
