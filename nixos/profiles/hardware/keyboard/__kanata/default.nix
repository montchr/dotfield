{ lib, config, ... }:
let
  inherit (builtins) readFile;
  cfg = config.services.kanata;

  kbdComment = ''
    ;; Machine-specific configurations can be appended to this initial
    ;; hardware-specific configuration, provided that each configuration share
    ;; the same `defsrc` block.
  '';
in
{
  config = {
    # Required: allow kernel-level input event interception
    # <https://github.com/jtroo/kanata/blob/v1.5.0/docs/avoid-sudo-linux.md>
    # <https://wiki.archlinux.org/title/Input_remap_utilities>
    dotfield.guardian.extraGroups = [
      "input"
      "uinput"
    ];

    # The NixOS module does not automatically make the package available.
    environment.systemPackages = [ cfg.package ];

    services.kanata = {
      enable = true;

      keyboards."default" = {
        config = ''
          ${kbdComment}
          ${readFile ./default.kbd}
        '';

        # TODO: exclude atreus + moonlander
        extraDefCfg = ''
          linux-dev-names-exclude (
            "Yubico YubiKey OTP+FIDO+CCID"
          )
        '';
      };
    };
  };
}
