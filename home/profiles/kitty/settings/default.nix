{inputs, ...}: let
  l = inputs.nixpkgs.lib // builtins;
in {
  imports = [./typography.nix];

  programs.kitty.settings = {
    #: Cursor customization {{{
    cursor_shape = "beam";
    cursor_beam_thickness = "1.5";
    cursor_underline_thickness = "2.0";
    cursor_blink_interval = "-1";
    # cursor_stop_blinking_after = "15.0";
    #: }}}

    #: Scrollback {{{
    scrollback_lines = "4000";
    scrollback_pager = "less --chop-long-lines --RAW-CONTROL-CHARS +INPUT_LINE_NUMBER"; # default value
    scrollback_pager_history_size = "666"; # in MB
    #: }}}

    #: Mouse {{{
    strip_trailing_spaces = "always";
    #: }}}

    #: Window layout {{{

    # sizing/spacing
    remember_window_size = true;
    window_padding_width = "10";
    window_margin_width = "0";
    single_window_margin_width = "-1";

    # decorations
    draw_minimal_borders = true;
    hide_window_decorations = true;
    confirm_os_window_close = "0";

    # layouts (preferred order)
    #
    # - First layout in list becomes the default layout.
    # - Order of layouts affects next/prev cycling order.
    # - Default value is all layouts in alphabetical order.
    # - Accepts a comma-separated string.
    #
    # https://sw.kovidgoyal.net/kitty/conf/#opt-kitty.enabled_layouts
    # https://sw.kovidgoyal.net/kitty/overview/#layouts
    enabled_layouts = l.concatStringsSep ", " [
      "tall"
      "grid"
      "horizontal"
      "vertical"
      "fat"
      "splits"
      "stack"
    ];

    #: }}}

    #: Tab bar {{{
    tab_bar_edge = "bottom";
    tab_bar_style = "fade";
    active_tab_font_style = "bold";
    inactive_tab_font_style = "normal";
    tab_activity_symbol = "";
    #: }}}

    #: Advanced {{{
    allow_remote_control = true;
    listen_on = "unix:/tmp/kitty-socket";
    # FIXME: why not?
    # startup_session = "session";
    #: }}}

    #: Keyboard shortcuts {{{
    kitty_mod = "ctrl+shift"; # default
    #: }}}

    #: Performance {{{
    # Prevent input latency.
    sync_to_monitor = false;
    #: }}}

    #: Terminal bell {{{
    enable_audio_bell = false;
    visual_bell_duration = "0.3";
    #: }}}

    #: OS-specific tweaks {{{
    # Set the titlebar background color to that of the currently-active window.
    wayland_titlebar_color = "background";
    #: }}}
  };
}
