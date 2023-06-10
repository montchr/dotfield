_: {
  services.xserver.enable = true;
  services.xserver.desktopManager.gnome.enable = true;
  services.gnome.games.enable = true;
  services.gnome.sushi.enable = true;
  # FIXME: has no effect, prob cos gpg-agent is configured in home-manager?
  programs.gnupg.agent.pinentryFlavor = "gnome3";
  # Required for browser integrations with GNOME services.
  services.gnome.gnome-browser-connector.enable = true;
}
