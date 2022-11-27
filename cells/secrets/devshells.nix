{inputs}: let
  inherit (inputs.std) std;
  inherit (inputs.agenix.packages) agenix;
  inherit
    (inputs.sops-nix.packages)
    sops-import-keys-hook
    sops-init-gpg-key
    ssh-to-pgp
    ;
  inherit (pkgs) rage ssh-to-age sops;
  l = inputs.nixpkgs.lib // builtins;
  pkgs = inputs.nixpkgs;
  category = "secrets";
  cmd = package: {inherit package category;};
in {
  secrets = std.lib.mkShell (_: {
    name = "dotfield-secrets";
    commands = [
      (cmd agenix)
      (cmd rage)
      (cmd sops)
      (cmd ssh-to-age)
      {
        name = "install-age-key";
        category = "secrets";
        help = "copy the age secret key from the password-store into place";
        command = ''
          mkdir -p $SOPS_AGE_KEY_DIR
          ${pkgs.pass}/bin/pass show age--secret-key >> $SOPS_AGE_KEY_FILE
        '';
      }
      {
        name = "convert-ssh-to-age-key";
        category = "secrets";
        help = "helper to convert the usual ssh ed25519 keys to age keys";
        command = ''
          mkdir -p $SOPS_AGE_KEY_DIR
          ${l.getExe ssh-to-age} -private-key -i ~/.ssh/id_ed25519 > $SOPS_AGE_KEY_DIR/age-key.sec
          ${l.getExe ssh-to-age} -i ~/.ssh/id_ed25519.pub > $SOPS_AGE_KEY_DIR/age-key.pub
        '';
      }
    ];
    packages = [
      sops-import-keys-hook
      sops-init-gpg-key
      ssh-to-pgp
    ];

    env = [
      {
        name = "SOPS_AGE_KEY_DIR";
        eval = "$XDG_CONFIG_HOME/sops/age";
      }
      {
        name = "SOPS_AGE_KEY_FILE";
        eval = "$XDG_CONFIG_HOME/sops/age/keys";
      }
      {
        name = "AGENIX_ROOT";
        eval = "$PRJ_ROOT";
      }
    ];
  });
}
