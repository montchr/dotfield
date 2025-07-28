{ self, lib, ... }:
let
  inherit (self.inputs) apparat;
  inherit (apparat.lib.devshell) pkg;
  category = "secrets";
  cmd = pkg category;
in
{
  perSystem =
    { inputs', pkgs, ... }:
    let
      inherit (inputs') sops-nix;
    in
    {
      devshells.dotfield-secrets = {
        devshell.name = "dotfield-secrets";
        devshell.packages = [
          sops-nix.packages.sops-import-keys-hook
          sops-nix.packages.ssh-to-pgp
          sops-nix.packages.sops-init-gpg-key
        ];

        commands = [
          (cmd pkgs.age-plugin-yubikey)
          (cmd pkgs.rage)
          (cmd pkgs.sops)
          (cmd pkgs.ssh-to-age)
          {
            inherit category;
            name = "sops-update-all-keys";
            help = "Sync all sops secrets files with configuration keys";
            command = ''
              fd 'secrets\.(yaml|yml)$' \
                -x sops updatekeys
            '';
          }
          {
            inherit category;
            name = "install-age-key";
            help = "copy the age secret key from the password-store into place";
            command = ''
              mkdir -p $SOPS_AGE_KEY_DIR
              ${pkgs.pass}/bin/pass show age--secret-key >> $SOPS_AGE_KEY_FILE
            '';
          }
          {
            inherit category;
            name = "convert-ssh-to-age-key";
            help = "helper to convert the usual ssh ed25519 keys to age keys";
            command = ''
              mkdir -p $SOPS_AGE_KEY_DIR
              ${lib.getExe pkgs.ssh-to-age} -private-key -i ~/.ssh/id_ed25519 > $SOPS_AGE_KEY_DIR/age-key.sec
              ${lib.getExe pkgs.ssh-to-age} -i ~/.ssh/id_ed25519.pub > $SOPS_AGE_KEY_DIR/age-key.pub
            '';
          }
        ];
      };
    };
}
