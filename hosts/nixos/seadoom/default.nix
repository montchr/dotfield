# FIXME: importables not available yet
# FIXME: is modulesPath available?

{ profiles, suites, modulesPath, ... }:
{
  imports = [
    (modulesPath + "/profiles/qemu-guest.nix")
    profiles.system.linode
  ] ++ suites.nixos;

  # TODO: remove or something
  # bud.enable = true;
  # bud.localFlakeClone = "/etc/nixos";

  boot.loader.grub.enable = true;
  boot.loader.grub.version = 2;

  boot.initrd.availableKernelModules = [ "virtio_pci" "virtio_scsi" "ahci" "sd_mod" ];
  boot.initrd.kernelModules = [ ];
  boot.kernelModules = [ ];
  boot.extraModulePackages = [ ];

  fileSystems."/" = {
    device = "/dev/sda";
    fsType = "ext4";
  };

  swapDevices = [
    { device = "/dev/sdb"; }
  ];

  time.timeZone = "America/New_York";

  users.users.xtallos = {
    isNormalUser = true;
    hashedPassword = "$6$yq7jJybfGyx19QqK$mr1dfKu1fChKkYDUZvQnlcKCmAYywIvWZXw3uT9EjQ/Xi85SGqkPDcsrrQ.7WEYM6InqDPqGZrTGfvoFpuONi1";
    extraGroups = [ "wheel" "network-manager" ]; # Enable ‘sudo’ for the user.
  };
  home-manager.users.xtallos = { pkgs, ... }: {
    home.packages = with pkgs; [
      ddate
    ];
    programs.fish.enable = true;
  };
  home-manager.useUserPackages = true;
  home-manager.useGlobalPkgs = true;

  users.users.root.hashedPassword = "$6$OZwXf5o.yZUoehTw$29dItBi5.fjd2GZToR6sP2F.xEUn/5TBKZlZ6CHNazEAWz6roIevLATX82QV9rCxlZ21sL9zsW..LmTFhC/dx.";

  users.mutableUsers = false;

  services.openssh.permitRootLogin = "yes";
  services.openssh.openFirewall = true;

  system.stateVersion = "21.05";
}
