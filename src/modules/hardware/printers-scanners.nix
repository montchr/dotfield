{
  dotfield.modules.workstation.nixos = {
    services.printing.enable = true;
    hardware.sane = {
      enable = true;
      openFirewall = true;
    };
  };
}
