{
  self,
  system,
  ...
}: let
  inherit (self.packages.${system}) emacs-plus;
in {
  environment.systemPackages = [
    (emacs-plus.override {
      withTitleBar = false;
      withDebug = true;
      icon = "modern-alecive-flatwoken";
    })
  ];
  homebrew.brews = [
    # :lang org (macOS only)
    "pngpaste"
  ];
}
