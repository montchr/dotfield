{profiles, ...}: {
  imports =
    (with (profiles.common); [
      networking.common
      networking.tailscale
      fonts.common
      fonts.pragmatapro
      secrets
      ssh-host
    ])
    ++ (with (profiles.darwin); [
      emacs
      gui
      system-defaults
    ]);
}
