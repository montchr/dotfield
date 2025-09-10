{
  aspects.workstation.nixos = {
    services.printing.enable = true;
    services.avahi.openFirewall = true;

    # scanner support
    hardware.sane = {
      enable = true;
      openFirewall = true;
    };
  };
}
