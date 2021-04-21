{ ... }:
{
  programs.kitty = {
    enable = true;

    settings = {
      font_family = "Iosevka Term";
      font_size = "14.0";
      adjust_line_height = "120%";
      disable_ligatures = "cursor"; # disable ligatures when cursor is on them

      # Window layout
      hide_window_decorations = "titlebar-only";
      window_padding_width = "10";

      # Tab bar
      tab_bar_edge = "top";
      tab_bar_style = "separator";
      tab_title_template = "Tab {index}: {title}";
      active_tab_font_style = "bold";
      inactive_tab_font_style = "normal";
    };
  };
}
