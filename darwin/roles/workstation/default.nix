{
  collective,
  profiles,
  ...
}: {
  imports =
    (with (collective.profiles); [
      fonts.common
      fonts.pragmatapro
      networking.common
      networking.ssh-host
      networking.tailscale
      secrets
    ])
    ++ (with profiles; [
      emacs
      gui
      system-defaults
    ]);
}
