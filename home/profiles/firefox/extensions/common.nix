{
  inputs,
  inputs',
  config,
  ...
}: let
  firefox-addons = inputs'.firefox-addons.packages;
  l = inputs.nixpkgs.lib // builtins;
in {
  programs.firefox.extensions =
    (with firefox-addons; [
      onepassword-password-manager
      a11ycss
      anchors-reveal
      auto-tab-discard
      bitwarden
      cookie-autodelete
      copy-selection-as-markdown
      darkreader
      # firefox-color
      link-cleaner
      multi-account-containers
      offline-qr-code-generator
      old-reddit-redirect
      org-capture
      raindropio
      react-devtools
      reddit-enhancement-suite
      reduxdevtools
      refined-github
      single-file
      tab-session-manager
      temporary-containers
      tree-style-tab
      tst-search
    ])
    ++ (l.optional config.programs.browserpass.enable firefox-addons.browserpass);
}
