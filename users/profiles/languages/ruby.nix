{config,lib,pkgs,...}:
let
  rubyHome = "${config.xdg.dataHome}/ruby";
  mkRuby = rubyPkg: {
    "ruby/versions/${rubyPkg.version.majMinTiny}".source = toString rubyPkg;
  };
in
{
  home.packages = with pkgs; [
    ruby
    rbenv
    rubocop
  ];

  home.sessionVariables = {
    RUBY_HOME = rubyHome;
    RBENV_ROOT = rubyHome;

    BUNDLE_USER_CACHE = "${config.xdg.cacheHome}/bundle";
    BUNDLE_USER_CONFIG = "${config.xdg.configHome}/bundle";
    BUNDLE_USER_PLUGIN = "${config.xdg.dataHome}/bundle";
  };

  xdg.dataFile = lib.mkMerge [
    (mkRuby pkgs.ruby_2_7)
  ];
}
