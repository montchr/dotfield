{pkgs, ...}: {
  # LISH compatibility
  boot.kernelParams = ["console=ttyS0,19200n8"];
  boot.loader.timeout = 10;
  boot.loader.grub.forceInstall = true;
  boot.loader.grub.device = "nodev";
  boot.loader.grub.extraConfig = ''
    serial --speed=19200 --unit=0 --word=8 --parity=no --stop=1;
    terminal_input serial;
    terminal_output serial
  '';

  networking.usePredictableInterfaceNames = false;
  networking.useDHCP = false;
  networking.interfaces.eth0.useDHCP = true;

  # Diagnostic tools used by Linode support.
  # TODO: provide citation
  environment.systemPackages = with pkgs; [
    inetutils
    mtr
    sysstat
  ];
}
