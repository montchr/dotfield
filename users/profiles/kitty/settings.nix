{ ... }:
{
  font_family = "PragmataPro Mono Liga";
  font_size = "16.0";
  # adjust_line_height = "110%";
  disable_ligatures = "cursor";
  # box_drawing_scale = "0.001, 1, 1.5, 2";

  #: Cursor customization {{{
  cursor_shape = "beam";
  cursor_beam_thickness = "1.5";
  cursor_underline_thickness = "2.0";
  cursor_blink_interval = "-1";
  cursor_stop_blinking_after = "15.0";
  #: }}}

  #: Scrollback {{{
  scrollback_lines = "15000";
  scrollback_pager = "bat";
  scrollback_pager_history_size = "200";
  #: }}}

  #: Window layout {{{
  remember_window_size = "yes";
  initial_window_width = "640";
  initial_window_height = "400";
  window_padding_width = "10";
  window_margin_width = "0";
  single_window_margin_width = "-1";
  draw_minimal_borders = "yes";
  hide_window_decorations = "yes";
  #: }}}

  #: Tab bar {{{
  tab_bar_edge = "bottom";
  tab_bar_style = "fade";
  active_tab_font_style = "bold";
  inactive_tab_font_style = "normal";
  tab_activity_symbol = "";
  #: }}}

  #: Advanced {{{
  allow_remote_control = "no";
  listen_on = "none";
  startup_session = "session";
  #: }}}

  #: OS specific tweaks {{{
  macos_quit_when_last_window_closed = "no";
  macos_custom_beam_cursor = "yes";
  #: }}}

  #: Keyboard shortcuts {{{
  kitty_mod = "ctrl+shift";
  #: }}}
}
