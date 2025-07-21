{
  flake.modules.nixos.workstation =
    { config, ... }:
    {
      # The NixOS module does not automatically make the package available.
      environment.systemPackages = [ config.services.kanata.package ];

      services.kanata = {
        enable = true;
        keyboards."default" = {
          # TODO: exclude atreus + moonlander
          extraDefCfg = ''
            ;; NOTE: `process-unmapped-keys' may cause issues with some keys.  But
            ;;       without it, all `defsrc' definitions would need to contain
            ;;       all available keys else some actions will not work properly.
            ;;       Most notably `tap-hold-release' will not react to the
            ;;       unmapped keys.
            ;;       <https://github.com/jtroo/kanata/blob/v1.6.0/docs/config.adoc#process-unmapped-keys>
            process-unmapped-keys yes

            linux-dev-names-exclude (
              "Yubico YubiKey OTP+FIDO+CCID"
            )
          '';
        };
      };
    };

  flake.modules.nixos.admin-user = {
    # Allow kernel-level input event interception
    # <https://github.com/jtroo/kanata/blob/v1.5.0/docs/avoid-sudo-linux.md>
    # <https://wiki.archlinux.org/title/Input_remap_utilities>
    dotfield.guardian.extraGroups = [
      "input"
      "uinput"
    ];
  };
}
