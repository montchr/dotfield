{
  dotfield.baseline.nixos =
    { config, pkgs, ... }:
    {
      environment.variables = {
        EDITOR = "vim";
        HOSTNAME = config.networking.hostName;
        LANG = "en_US.UTF-8";
        LC_ALL = "en_US.UTF-8";
        XDG_CACHE_HOME = "$HOME/.cache";
        XDG_CONFIG_HOME = "$HOME/.config";
        XDG_DATA_HOME = "$HOME/.local/share";
        XDG_STATE_HOME = "$HOME/.local/state";

        # Although it points to a commonly-used path for user-owned executables,
        # $XDG_BIN_HOME is a non-standard environment variable. It is not part of
        # the XDG Base Directory Specification.
        # https://specifications.freedesktop.org/basedir-spec/basedir-spec-latest.html
        XDG_BIN_HOME = "$HOME/.local/bin";
      };
    };
}
