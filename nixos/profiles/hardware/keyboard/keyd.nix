## <https://github.com/rvaiya/keyd>
{ config, pkgs, ... }:
{
  assertions = [
    {
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

      global = {
        # Ignore the tap behaviour of an overloaded key if it is held for the
        # given number of miliseconds.
        overload_tap_timeout = 500;
      };

      # based on recommended config <https://github.com/rvaiya/keyd#recommended-config>
      main = {
        # control = "oneshot(control)";

        capslock = "overload(control, esc)";
      };
    };
  };
}
