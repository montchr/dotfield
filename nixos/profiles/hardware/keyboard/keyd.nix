{ config, pkgs, ... }:
{
  assertions = [
    {
      # FIXME: probably needs an `or`, incase the option is missing altogether
      assertion = !(config.services.kmonad.enable);
      message = "kmonad conflicts with keyd";
    }
  ];

  environment.systemPackages = [
    pkgs.keyd # services.keyd module has no package option
  ];

  services.keyd.enable = true;
  services.keyd.keyboards.default = {
    ids = [ "*" ];
    settings = {
      # based on recommended config <https://github.com/rvaiya/keyd#recommended-config>
      # FIXME: something here causes super to not work? possibly related to
      # apple keyboard on hodgepodge?
      main = {
        # FIXME: not work?
        # shift = "oneshot(shift)";
        # meta = "oneshot(meta)";
        # control = "oneshot(control)";

        # leftalt = "oneshot(alt)";
        # rightalt = "oneshot(altgr)"; # TODO: do we want this?

        capslock = "overload(control, esc)";

        # insert = "S-insert";
      };
    };
  };
}
