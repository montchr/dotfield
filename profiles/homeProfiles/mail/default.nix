{pkgs, ...}: {
  home.packages = [
    pkgs.aerc
    pkgs.isync
    pkgs.mu
    pkgs.mutt
  ];

  programs.emacs.extraPackages = _epkgs: [
    pkgs.mu
  ];
}
