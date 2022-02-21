{ options, config, lib, pkgs, inputs, ... }:

let
  inherit (inputs) nix-colors;
  inherit (config) my;
  inherit (pkgs.stdenv) isDarwin isLinux;

  user = builtins.getEnv "USER";
  name = if builtins.elem user [ "" "root" ] then my.username else user;

  # Note that `builtins.getEnv` will only return an empty string unless running
  # an impure build. For that reason, a fallback value is necessary.
  envTheme = builtins.getEnv "DOTFIELD_THEME";
  theme = if envTheme != "" then envTheme else "black-metal-khold";
in

{
  users.users.${my.username} = lib.mkAliasDefinitions options.my.user;

  colorscheme = nix-colors.colorSchemes.${theme};

  my.user = {
    inherit name;

    shell = pkgs.zsh;
    packages = import ./package-list.nix { inherit pkgs; };
  }
  //
  (lib.optionalAttrs (isLinux) {
    extraGroups = [ "wheel" ];
    isNormalUser = true;
    home = "/home/${name}";
  })
  //
  (lib.optionalAttrs (isDarwin) {
    home = "/Users/${name}";
  });

  my.keys.ssh = {
    primary = (import ./ssh-primary-key.nix);
    identities = (import ./ssh-identities.nix);
  };

  environment.variables = let inherit (my) xdg; in
    {
      CACHEDIR = xdg.cache;
      XDG_BIN_HOME = xdg.bin;
      XDG_CACHE_HOME = xdg.cache;
      XDG_CONFIG_HOME = xdg.config;
      XDG_DATA_HOME = xdg.data;
      XDG_RUNTIME_DIR = "/tmp";
      XDG_STATE_HOME = xdg.state;
    };

  my.env = {
    # Appearance
    BASE16_THEME_DARK = "black-metal-khold";
    BASE16_THEME_LIGHT = "grayscale-light";
    DOTFIELD_EMACS_THEME_DARK = "modus-vivendi";
    DOTFIELD_EMACS_THEME_LIGHT = "modus-operandi";

    # Default is "1". But when typeset in PragmataPro that leaves no space
    # between the icon and its filename.
    EXA_ICON_SPACING = "2";

    GITHUB_USER = my.githubUsername;
    Z_OWNER = my.username;
  };


  my.hm.home = {
    # Necessary for home-manager to work with flakes, otherwise it will
    # look for a nixpkgs channel.
    stateVersion =
      if pkgs.stdenv.isDarwin then
        "21.11"
      else
        config.system.stateVersion;
    username = my.username;
  };

  my.hm.xdg.enable = true;

  home-manager = {
    useGlobalPkgs = true;
    useUserPackages = true;
    users.${my.username} = lib.mkAliasDefinitions options.my.hm;
  };
}
