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

          # FIXME: failed to build on `aarch64-darwin`
          #       error: builder for '/nix/store/czsmbx30im50gx58jsd8pz0gm1d8slja-perl5.36.0-po4a-0.62.drv' failed with exit code 255;
          #        last 10 log lines:
          #        > t/cfg-single.t      (Wstat: 2048 (exited 8) Tests: 52 Failed: 8)
          #        >   Failed tests:  1-8
          #        >   Non-zero exit status: 8
          #        > t/cfg-split.t       (Wstat: 2048 (exited 8) Tests: 40 Failed: 8)
          #        >   Failed tests:  1-8
          #        >   TODO passed:   12, 32
          #        >   Non-zero exit status: 8
          #        > Files=29, Tests=309, 68 wallclock secs ( 0.21 usr  0.09 sys + 35.97 cusr 28.52 csys = 64.79 CPU)
          #        > Result: FAIL
          #        > Failed 3/29 test programs. 24/309 subtests failed.
          #        For full logs, run 'nix log /nix/store/czsmbx30im50gx58jsd8pz0gm1d8slja-perl5.36.0-po4a-0.62.drv'.
          # error: 1 dependencies of derivation '/nix/store/8nqfl19fl2h556mf1bv2bx2qrn6p8za2-util-linux-2.39.drv' failed to build
          # error: 1 dependencies of derivation '/nix/store/67l5lbha5rvdpvcz8g3d6is81pvsxyy3-sops-init-gpg-key.drv' failed to build
          # sops-nix.packages.sops-init-gpg-key
        ];

        commands = [
          (cmd pkgs.age-plugin-yubikey)
          (cmd pkgs.rage)
          (cmd pkgs.sops)
          (cmd pkgs.ssh-to-age)
          # FIXME: broken on nixos-unstable 2024-05-05
          # (cmd pkgs.yubikey-manager)
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
