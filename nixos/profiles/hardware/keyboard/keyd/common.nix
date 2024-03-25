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

  ## NOTES:
  #
  # keyd mod names may be different from expectations:
  #
  #   C =>	Ctrl
  #   M =>	Meta	aka "Super" (Emacs, GNOME) or "Command" (macOS) or "Windows" (MSWin)
  #   A =>	Alt	aka "Meta" (Emacs) or "Option" (macOS)
  #   S =>	Shift
  #   G =>	AltGr
  #
  # the docs tend to list these in the order C-M-A-S-G.
  #
  # and so, the equivalent composite mods:
  #
  #   "Hyper"	Ctrl + Meta + Alt + Shift	C-M-A-S
  #   "Meh"	Ctrl + Alt + Shift		C-A-S
  services.keyd.keyboards.default = {
    ids = [ "*" ];
    settings = {
      global = {
        # Illuminate the capslock light whenever a layer is active.
        # We don't use capslock normally, ever, but even if we did, we would
        # still consider it just another layer (effectively a one-shot layer).
        layer_indicator = 1;

        # Ignore the tap behaviour of an overloaded key if it is held for the
        # given number of miliseconds.
        overload_tap_timeout = 500;
      };

      main = {
        capslock = "overload(control, esc)";
      };

      # Composite modifier aliases should be defined as "layers".
      "hyper:C-M-A-S" = { };
      "meh:C-A-S" = { };
    };
  };
}
