{
  moduleWithSystem,
  inputs,
  config,
  ...
}:
{
  hosts.nixos.riebeck = {
    system = "x86_64-linux";
    channel = "nixos-unstable";
    aspects = with config.aspects; [
      hardware__lenovo__thinkpad-x1-13th-gen
      workstation
      desktop-sessions__niri
      development__kleinweb
    ];
  };

  hosts.nixos.riebeck.configuration = moduleWithSystem (
    perSystem@{ config }:
    { config, pkgs, ... }:
    {
      boot.loader.systemd-boot.enable = true;
      boot.loader.efi.canTouchEfiVariables = true;
      # NOTE: Due to the this machine being initially-provisioned for
      # Windows, the boot partition is small (200MiB), which is very
      # much a Microsoft-ism (i.e. unreasonable).  While dual-boot is
      # obviously possible, we are limited to a very small number of
      # NixOS initrd images stored in the EFI filesystem.  Such a small
      # number could easily result in a state where it is not possible
      # to roll back to a known-good NixOS generation during a period of
      # active iteration where the bootloader is updated more than two
      # times in a single boot session.  That is, quite frankly, very
      # easy to hit when running `nixos-rebuild switch`.  Because the
      # NTFS Windows system partition is only ~16MiB away from the end
      # of the EFI partition, it is not possible to allocate more free
      # space for a re-created EFI partition without altering the start
      # sector for the NTFS partition (moving the start sector from the
      # left to the right), which is very likely to result in corruption
      # of the Windows operating system files. I already did that once
      # when I mistakenly tried to shrink the Windows partition from the
      # left to the right via GParted -- the entire Windows partition
      # then had to be deleted and re-provisioned by Klein IT support
      # staff.  That would have been fine, but the expanded EFI
      # partition I had created was accidentally deleted in the process
      # due to human error, thus resulting in the same tiny-EFI state I
      # was in before.  The second time around, I was able to shrink the
      # NTFS partition from within Windows, but the shrink moves the end
      # sector from right to left, which resulted in the desired total
      # space allocation for the EFI partition to be sandwiching the
      # NTFS partition.  The Windows disk management tool does not allow
      # for "moving" partitions, so I had to use GParted.  When I
      # attempted to move the NTFS partition to the right, however,
      # GParted warned that this would likely result in a corrupted
      # filesystem.  Not an experience I would like to put myself in
      # again.  One solution to the limited generation capacity in the
      # EFI partition could be storing the kernels and initrd on the
      # primary Linux partition, but that would require switching from
      # systemd-boot to GRUB.  Perhaps a project for a rainy day, as
      # long as there's nothing in my configuration that requires
      # systemd-boot.  For more on this subject, refer to:
      #
      # <https://wiki.nixos.org/wiki/Bootloader#Keeping_kernels/initrd_on_the_main_partition>
      boot.loader.systemd-boot.configurationLimit = 3;

      boot.kernelPackages = pkgs.linuxPackages_latest;

      # Enable networking
      networking.networkmanager.enable = true;

      # Set your time zone.
      time.timeZone = "America/New_York";

      # Select internationalisation properties.
      i18n.defaultLocale = "en_US.UTF-8";

      i18n.extraLocaleSettings = {
        LC_ADDRESS = "en_US.UTF-8";
        LC_IDENTIFICATION = "en_US.UTF-8";
        LC_MEASUREMENT = "en_US.UTF-8";
        LC_MONETARY = "en_US.UTF-8";
        LC_NAME = "en_US.UTF-8";
        LC_NUMERIC = "en_US.UTF-8";
        LC_PAPER = "en_US.UTF-8";
        LC_TELEPHONE = "en_US.UTF-8";
        LC_TIME = "en_US.UTF-8";
      };

      # List packages installed in system profile. To search, run:
      # $ nix search wget
      environment.systemPackages = with pkgs; [
        vim # Do not forget to add an editor to edit configuration.nix! The Nano editor is also installed by default.
        git
        fd
        ripgrep
        tealdeer
        emacs-pgtk
        curl
        jq
        vscode
        rsync
        openssh
        wget
      ];

      fonts.packages = [ perSystem.config.packages.berkeley-mono ];

      # This value determines the NixOS release from which the default
      # settings for stateful data, like file locations and database versions
      # on your system were taken. It‘s perfectly fine and recommended to leave
      # this value at the release version of the first install of this system.
      # Before changing this value read the documentation for this option
      # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
      system.stateVersion = "25.05"; # Did you read the comment?

    }
  );
}
