{
  config,
  lib,
  pkgs,
  suites,
  profiles,
  hmUsers,
  ...
}: {
  imports = with suites;
    graphical
    ++ personal
    ++ (with profiles; [
      audio
      users.xtallos
      virtualisation.guests.parallels
    ])
    ++ [./hardware-configuration.nix];

  users.users.xtallos = {
    password = "xtallos";
    openssh.authorizedKeys.keys = [
      "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAACAQC3tWcOwvNOfHXX3YvtLmJRigxATUh++bWRCAM07uy3mbNvEteT5bF/7nixO44gep0Hv24jaqLeGjCaTxFXrmt1NGgvmAXcsoS4I3+N2xfiFZPIKoiF0EONDsInjm4h5eNoPPE4Rd9xLju4S4tXaXDcL37PunQZJ+aR6CRVf/geM+H4y70cvYHV6uakMAfuv/0+AEMLwlSIN7OpDN8B+JGI4rQhBsekRkkkcZlPYO4vT63aTvLCYFxJ/fR45oMKW57lvZUrbRMHbKRkOfyhBF3qbYR/9aMEUd7gjYBfLJ1hQaHlp2aV49m53WFBjmjqjFcxDPxS/HMk/Hazowkw0G6iNzSNHnO5wI/BxIEahavYvd4VOQXpaWs/G58t8kdQol8WFufLjAReP0j16TqcWEHwy1ktMcrpYfDlLSlNcuaUeXJNIyvD3WmfRDXBnxlBenFIqe9lnK8RUVCcxM+lEEJbMWs1ZuWmgXjbt3UkFhSKSv2Adlm2/OfBBCyO46hVmhLfkwzB69aXYqUjPthlvtCDuLxrmT+DZeWsucUKPp2L9PXS6LpbpnIWCqmnGIPLjHBX2X3EOKwrtLAGN5wv7zLv88qHOD0MET2KVZkfTLg04FkcNowNwAlQ8xBBjpt6xEWNFMH532ZRO1CT0VTUNB7nEW2JET1SULsRT/bTUbKQHQ== yubikey5cnfc"
      "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAACAQDPIGPzLVIvcbIzyrYEcx7NmAZjYm9cfQzLsDdnRBGJqw0mm0qtSUACor/d6jN1mreefpnzK+aXJ7ib6BPtFw7Xigi0c9qfXj2fmjx6XZHuYF+WhbyCgkUpqTWgVwdqEC89imiCImNXkDLotZDGSEV5sQVYOA2Y/E3H/Cur2ztQN40TJpf+nyHkmHmoA6PFDJ88wlabMzSFtl3p4w1hvtatlZ8HlAJl5J1H4gjDa52FkZelx+QkhRGthxqg8MXmWCUZBggq2n+f/F8nSSyh0JGnbc8fKZTpQ2Xv0X021b0RLjBOTFH4QkYgshvBrpTUCI1nN2iamxvHSVka8VheTzm0cU3rJuU0UITuBr1KjgyVaS8Jce8dDB2B70ZK34fFCcrO6Kz+fSB0HTU1JkT0RT9pLdgQxKis68vGTaVvw2/ZnXOwsWvqb0rrQTUHNyfhYoXcdjTdu/OYe6LKkQxNjA8N82SaS0C6CabeAfs7U+RV6qArHPjRPgwYlJApDHh6bYpios5+ieOUBoxVgmxT24V0+x0KTBmWKNEVcJkFnZx9i+2Dv2brhZdmnBsF/C6M9RXW849pIPpU/MX3tmDA3BK5UxjExeEfNYy1lt3rHdLF1dKLthZAptdFPaifxi+gltHGfY0T4A2yy+b0ZObvIVOGKdl51ijOmCumG1dI4GRqxw== ShellFish.tso@aerattum"
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIA4jOwrH1r3EUsIZ5Uo0j+JAFFJ/9HK+NDiPAVWNYNEg seadoom.tso@blink"
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIGk9fhwXG95cVD9DLsHuXrdJYs8DsUF/AmYWcO1+bPVd alleymon"
    ];
  };

  home-manager.users.xtallos = {profiles, suites, ...}: {
    imports =
      [hmUsers.xtallos]
      ++ (with suites; graphical)
      ++ (with profiles; [mail]);
  };

  boot.loader.grub.enable = true;
  boot.loader.grub.version = 2;
  boot.loader.grub.device = "/dev/sda";
  boot.initrd.checkJournalingFS = false;

  networking.useDHCP = false;
  networking.interfaces.enp0s5.useDHCP = true;

  services.openssh.enable = true;
  # TODO: should this be locked down further?
  services.openssh.openFirewall = true;

  security.sudo.enable = true;
  security.sudo.wheelNeedsPassword = false;

  security.doas.enable = true;
  security.doas.wheelNeedsPassword = false;

  environment.variables.DOTFIELD_DIR = "/etc/nixos";

  users.mutableUsers = false;

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = with pkgs; [
    _1password-gui
    coreutils
    fd
    firefox
    git
    httpie
    kitty
    ripgrep
    vim
    vscodium
    wget
  ];

  networking.firewall.enable = false;

  system.stateVersion = "21.11";
}
