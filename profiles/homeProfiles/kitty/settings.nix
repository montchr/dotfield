{flake, ...}: let
  l = flake.inputs.nixpkgs.lib // builtins;
in {
  programs.kitty.settings = {
    ##: --- cursor customization ---

    cursor_shape = "beam";
    cursor_beam_thickness = "1.5";
    cursor_underline_thickness = "2.0";
    cursor_blink_interval = "-1";
    macos_custom_beam_cursor = "yes";

    ##: --- scrollback ---

    scrollback_lines = "4000";
    scrollback_pager = "less --chop-long-lines --RAW-CONTROL-CHARS +INPUT_LINE_NUMBER"; # default value
    scrollback_pager_history_size = "666"; # in MB

    ##: --- mouse ---

    strip_trailing_spaces = "always";

    ##: --- frames ---

    ##: sizing/spacing
    remember_window_size = true;
    window_padding_width = "10";
    window_margin_width = "0";
    single_window_margin_width = "-1";

    ##: decorations
    draw_minimal_borders = true;
    hide_window_decorations = true;
    confirm_os_window_close = "0";
    # linux/wayland: set the titlebar background color to that of the currently-active window
    wayland_titlebar_color = "background";

    ##: --- windows ---

    ##: layouts (preferred order)
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

    ##: --- tabs ---

    tab_bar_edge = "bottom";
    tab_bar_style = "fade";
    active_tab_font_style = "bold";
    inactive_tab_font_style = "normal";
    tab_activity_symbol = "";

    ##: --- keyboard ---

    kitty_mod = "ctrl+shift"; # default
    # macOS: make keyboard shortcuts usable again
    # https://sw.kovidgoyal.net/kitty/conf/#opt-kitty.macos_option_as_alt
    macos_option_as_alt = "left"; # => "left" | "right" | "both" | false;

    ##: --- sessions ---

    allow_remote_control = true;
    listen_on = "unix:/tmp/kitty-socket";

    ##: --- misc ---

    # Prevent input latency.
    sync_to_monitor = false;

    ##: bell
    enable_audio_bell = false;
    visual_bell_duration = "0.3";
  };
}
