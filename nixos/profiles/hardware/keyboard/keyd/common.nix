# <https://github.com/rvaiya/keyd>
# <https://github.com/NixOS/nixpkgs/issues?q=is%3Aissue+is%3Aopen+sort%3Aupdated-desc+keyd>
# <https://github.com/NixOS/nixpkgs/issues/284797>
{ config, pkgs, ... }:
{
  assertions = [
    {
      assertion = !(config.services.kmonad.enable);
      message = "kmonad conflicts with keyd";
    }
  ];

  # XXX: services.keyd module has no package option
  environment.systemPackages = [ pkgs.keyd ];

  # XXX: <https://github.com/NixOS/nixpkgs/issues/290161>
  users.groups.keyd = { };
  systemd.services.keyd.serviceConfig.CapabilityBoundingSet = [ "CAP_SETGID" ];

  dotfield.guardian.extraGroups = [ "keyd" ];

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
    # XXX: the module -- which is very broken -- does not produce a valid file
    # because it has no regard for the ordering of elements within when merging
    # importing attrs. better just use the conf like normal people.
    extraConfig = builtins.readFile ./default.extra.conf;
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
    };
  };
}
