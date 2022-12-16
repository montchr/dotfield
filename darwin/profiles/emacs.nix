{
  self,
  system,
  ...
}: let
  inherit (self.packages.${system}) emacs-plus-edge;
in {
  environment.systemPackages = [
    emacs-plus-edge

    # (emacs-plus.override {
    #   withTitleBar = false;
    #   # withDebug = true;
    #   icon = "modern-alecive-flatwoken";
    # })
  ];
}
