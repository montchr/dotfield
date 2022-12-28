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
      add-custom-search-engine
      bitwarden
      copy-selection-as-markdown
      darkreader
      display-_anchors
      firefox-color
      kagi-search
      multi-account-containers
      octolinker
      old-reddit-redirect
      org-capture
      raindropio
      react-devtools
      reddit-enhancement-suite
      reduxdevtools
      refined-github
      sidebery
      single-file
      tab-session-manager
      temporary-containers
    ])
    ++ (l.optional config.programs.browserpass.enable firefox-addons.browserpass);
}
