{
  imports = (import ../../home/features.nix).workstation ++ [
    ../../home/profiles/desktop/applications/microsoft-teams.nix
    ../../home/profiles/development/work/default.nix
    ../../home/profiles/gpg/with-ssh-support.nix
    ../../home/profiles/shells/fish/trampoline.nix
    ../../home/profiles/shells/fish/with-fifc-completion.nix
    ../../home/profiles/shells/prompts/starship/default.nix

    {
      # The trackpad on this device is huge, and I always end up touching
      # its corner with my palm, which is very disruptive.
      dconf.settings."org/gnome/desktop/peripherals/touchpad".tap-to-click = false;
    }
  ];
  home.stateVersion = "23.05";
}
