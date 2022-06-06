{
  config,
  lib,
  pkgs,
}: {
  font_family = "PragmataPro Mono Liga";
  font_size = "14.0";
  adjust_line_height = "110%";
  # TODO: why?
  box_drawing_scale = "0.001, 1, 1.5, 2";

  #: Cursor customization {{{
  cursor_shape = "beam";
  cursor_beam_thickness = "1.5";
  cursor_underline_thickness = "2.0";
  cursor_blink_interval = "-1";
  # cursor_stop_blinking_after = "15.0";
  #: }}}

  #: Scrollback {{{
  scrollback_lines = "4000";
  scrollback_pager = "less";
  scrollback_pager_history_size = "666"; # in MB
  #: }}}

  #: Window layout {{{
  remember_window_size = "no";
  initial_window_width = "640";
  initial_window_height = "800";
  window_padding_width = "10";
  window_margin_width = "0";
  single_window_margin_width = "-1";
  draw_minimal_borders = "yes";
  hide_window_decorations = lib.hm.booleans.yesNo config.wayland.windowManager.sway.enable;
  confirm_os_window_close = "0";
  #: }}}

  #: Tab bar {{{
  tab_bar_edge = "bottom";
  tab_bar_style = "fade";
  active_tab_font_style = "bold";
  inactive_tab_font_style = "normal";
  tab_activity_symbol = "";
  #: }}}

  #: Advanced {{{
  # allow_remote_control = "no";
  # listen_on = "none";
  # startup_session = "session";
  #: }}}

  #: Keyboard shortcuts {{{
  # This is the default value.
  # kitty_mod = "ctrl+shift";
  #: }}}
}
