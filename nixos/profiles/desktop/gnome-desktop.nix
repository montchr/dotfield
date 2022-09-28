{...}: {
  services.xserver.enable = true;
  services.xserver.desktopManager.gnome.enable = true;
  services.gnome.games.enable = true;
  services.gnome.sushi.enable = true;
  # FIXME: has no effect, prob cos gpg-agent is configured in home-manager?
  programs.gnupg.agent.pinentryFlavor = "gnome3";
  # Required for Firefox integration in home-manager
  services.gnome.chrome-gnome-shell.enable = true;
}
