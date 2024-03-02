{
  services.printing.enable = true;

  services.avahi = {
    enable = true;
    nssmdns4 = true;
    openFirewall = true;
  };

  # For scanner support.
  hardware.sane = {
    enable = true;
    openFirewall = true;
  };
}
