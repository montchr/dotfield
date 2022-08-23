{
  collective,
  profiles,
}: let
  workstation =
    (with (collective.profiles); [
      fonts.common
      networking.common
      networking.ssh-host
      networking.tailscale
      secrets
    ])
    ++ (with profiles; [
      emacs
      graphical.common
      # graphical.hammerspoon
      # graphical.sketchybar
      graphical.yabai
      system-defaults
    ]);
in {
  inherit
    workstation
    ;
}
