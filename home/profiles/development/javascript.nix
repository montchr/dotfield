{pkgs, ...}: {
  home.packages = with pkgs; [
    nodePackages.terser
  ];
}
