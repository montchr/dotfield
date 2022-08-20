{profiles, ...}: {
  imports =
    (with (profiles.shared); [
      networking.common
      networking.tailscale
      fonts.common
      fonts.pragmatapro
      secrets
      ssh-host
    ])
    ++ (with (profiles.system); [
      emacs
      gui
      system-defaults
    ]);
}
