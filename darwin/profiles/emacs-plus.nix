{
  inputs,
  config,
  ...
}: let
  inherit (config.homebrew) brewPrefix;
  l = inputs.nixpkgs.lib // builtins;
in {
  environment.systemPath = l.mkBefore [
    # NOTE: This is NOT a "prefix" in the usual sense -- it points to the `bin` directly...
    brewPrefix
  ];
  homebrew.taps = ["d12frosted/emacs-plus"];
  homebrew.brews = [
    {
      name = "emacs-plus@30";
      args = [
        "with-debug"
        "with-imagemagick"
        "with-mailutils"
        "with-native-comp"
        "with-modern-papirus-icon"
      ];
      start_service = true;
      restart_service = true;
      link = true;
    }
  ];
}
