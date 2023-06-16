{
  pkgs,
  flake,
  ...
}: let
  inherit (flake.inputs.apparat.lib.darwin) platformBrewPrefix;
  l = flake.inputs.nixpkgs.lib // builtins;

  brewPrefix = platformBrewPrefix pkgs.stdenv.hostPlatform;
in {
  # <https://github.com/LnL7/nix-darwin/issues/596>
  #
  # $ brew shellenv
  # export HOMEBREW_PREFIX="/opt/homebrew";
  # export HOMEBREW_CELLAR="/opt/homebrew/Cellar";
  # export HOMEBREW_REPOSITORY="/opt/homebrew";
  # export PATH="/opt/homebrew/bin:/opt/homebrew/sbin${PATH+:$PATH}";
  # export MANPATH="/opt/homebrew/share/man${MANPATH+:$MANPATH}:";
  # export INFOPATH="/opt/homebrew/share/info:${INFOPATH:-}";
  environment.systemPath = l.mkBefore ["${brewPrefix}/bin" "${brewPrefix}/sbin"];
  environment.variables = {
    HOMEBREW_PREFIX = brewPrefix;
    HOMEBREW_CELLAR = "${brewPrefix}/Cellar";
    HOMEBREW_REPOSITORY = brewPrefix;
    INFOPATH = "${brewPrefix}/share/info:\${INFOPATH:-}";
    MANPATH = "${brewPrefix}/share/man\${MANPATH+:$MANPATH}:";
  };

  homebrew = {
    enable = true;
    # Use the nix-darwin brewfile when invoking `brew bundle` imperatively.
    global.brewfile = true;
  };
}
