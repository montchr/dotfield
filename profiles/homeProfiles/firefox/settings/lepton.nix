##: Notes
#
# - If you want to disable or change a setting you have set previously, it MUST
#   be set explicitly.
#
#   E.G. To disable a setting currently set to `true`, it must be set to
#   `false`. *Do not simply delete the setting from this file!*.
#
#   This happens because the settings will be kept in browser preferences state.
#
#   Alternatively, you can go to `about:config` and reset the setting.
{lib, ...}: {
  imports = [./lepton-linux.nix];

  ### === userChrome ==========================================================
  # ** Theme Default Options ****************************************************
  # userchrome.css usercontent.css activate
  "toolkit.legacyUserProfileCustomizations.stylesheets" = true;

  # Proton Enabled #127 || Removed at 97 #328 (Maintained for compatibility with ESR)
  "browser.proton.enabled" = true;

  # Fill SVG Color
  "svg.context-properties.content.enabled" = true;

  # CSS Color Mix - 88 Above
  "layout.css.color-mix.enabled" = true;

  # CSS Blur Filter - 88 Above
  "layout.css.backdrop-filter.enabled" = true;

  # Restore Compact Mode - 89 Above
  "browser.compactmode.show" = true;

  # about:home Search Bar - 89 Above
  "browser.newtabpage.activity-stream.improvesearch.handoffToAwesomebar" = false;

  # CSS's `:has()` selector #457 - 103 Above
  "layout.css.has-selector.enabled" = true;

  # Browser Theme Based Scheme - Will be activate 95 Above
  "layout.css.prefers-color-scheme.content-override" = 3;

  # ** Theme Related Options ****************************************************

  # == Theme Distribution Settings ==============================================

  # The rows that are located continuously must be changed `true`/`false` explicitly because there is a collision.
  # https://github.com/black7375/Firefox-UI-Fix/wiki/Options#important
  "userChrome.tab.connect_to_window" = true; # Original, Photon
  "userChrome.tab.color_like_toolbar" = true; # Original, Photon

  "userChrome.tab.lepton_like_padding" = true; # Original
  "userChrome.tab.photon_like_padding" = false; # Photon

  "userChrome.tab.dynamic_separator" = true; # Original, Proton
  "userChrome.tab.static_separator" = false; # Photon
  "userChrome.tab.static_separator.selected_accent" = false; # Just option
  "userChrome.tab.bar_separator" = false; # Just option

  "userChrome.tab.newtab_button_like_tab" = true; # Original
  "userChrome.tab.newtab_button_smaller" = false; # Photon
  "userChrome.tab.newtab_button_proton" = false; # Proton

  "userChrome.icon.panel_full" = true; # Original, Proton
  "userChrome.icon.panel_photon" = false; # Photon

  # Original Only
  "userChrome.tab.box_shadow" = true;
  "userChrome.tab.bottom_rounded_corner" = true;

  # Photon Only
  "userChrome.tab.photon_like_contextline" = false;
  "userChrome.rounding.square_tab" = false;

  # == Theme Compatibility Settings =============================================
  # "userChrome.compatibility.accent_color" =         true; # Firefox v103 Below
  # "userChrome.compatibility.covered_header_image" = true;
  # "userChrome.compatibility.panel_cutoff" =         true;
  # "userChrome.compatibility.navbar_top_border" =    true;
  # "userChrome.compatibility.dynamic_separator" = false; # Need dynamic_separator

  # "userChrome.compatibility.os.linux_non_native_titlebar_button" = true;
  # "userChrome.compatibility.os.windows_maximized" = true;
  # "userChrome.compatibility.os.win11" =             true;

  # == Theme Custom Settings ====================================================

  # -- User Chrome --------------------------------------------------------------

  "userChrome.theme.proton_color.dark_blue_accent" = true;
  "userChrome.theme.monospace" = lib.mkDefault true;

  # "userChrome.decoration.disable_panel_animate" =    true;
  # "userChrome.decoration.disable_sidebar_animate" =  true;
  # "userChrome.decoration.panel_button_separator" =   true;
  "userChrome.decoration.panel_arrow" = false;

  "userChrome.autohide.tab" = false;
  # "userChrome.autohide.tab.opacity" =                true;
  # "userChrome.autohide.tab.blur" =                   true;
  "userChrome.autohide.tabbar" = false;
  "userChrome.autohide.navbar" = false;
  "userChrome.autohide.bookmarkbar" = false;
  "userChrome.autohide.sidebar" = false;
  "userChrome.autohide.fill_urlbar" = false;
  "userChrome.autohide.back_button" = false;
  "userChrome.autohide.forward_button" = false;
  "userChrome.autohide.page_action" = false;
  "userChrome.autohide.toolbar_overlap" = false;
  "userChrome.autohide.toolbar_overlap.allow_layout_shift" = false;

  # "userChrome.hidden.tab_icon" =                     true;
  # "userChrome.hidden.tab_icon.always" =              true;
  "userChrome.hidden.tabbar" = false;
  "userChrome.hidden.navbar" = false;
  # "userChrome.hidden.titlebar_container" =           true;
  "userChrome.hidden.sidebar_header" = true;
  # "userChrome.hidden.sidebar_header.vertical_tab_only" = true;
  # "userChrome.hidden.urlbar_iconbox" =               true;
  # "userChrome.hidden.urlbar_iconbox.label_only" =    true;
  "userChrome.hidden.bookmarkbar_icon" = false;
  "userChrome.hidden.bookmarkbar_label" = false;
  # "userChrome.hidden.disabled_menu" =                true;

  ##: Private Browsing
  # Use purple color in private mode.
  "userChrome.theme.private" = true;
  "userChrome.hidden.private_indicator" = true;

  # "userChrome.centered.tab" =                        true;
  # "userChrome.centered.tab.label" =                  true;
  # "userChrome.centered.urlbar" =                     true;
  # "userChrome.centered.bookmarkbar" =                true;

  # "userChrome.counter.tab" =                         true;
  # "userChrome.counter.bookmark_menu" =               true;

  "userChrome.combined.nav_button" = false;
  "userChrome.combined.nav_button.home_button" = false;
  "userChrome.combined.urlbar.nav_button" = false;
  "userChrome.combined.urlbar.home_button" = false;
  "userChrome.combined.urlbar.reload_button" = false;
  # "userChrome.combined.sub_button.none_background" = true;
  # "userChrome.combined.sub_button.as_normal" =       true;

  "userChrome.rounding.square_button" = true;
  "userChrome.rounding.square_dialog" = true;
  "userChrome.rounding.square_panel" = true;
  "userChrome.rounding.square_panelitem" = true;
  "userChrome.rounding.square_menupopup" = true;
  "userChrome.rounding.square_menuitem" = true;
  "userChrome.rounding.square_infobox" = true;
  "userChrome.rounding.square_toolbar" = true;
  "userChrome.rounding.square_field" = true;
  "userChrome.rounding.square_urlView_item" = true;
  "userChrome.rounding.square_checklabel" = true;

  # "userChrome.padding.first_tab" =                   true;
  # "userChrome.padding.first_tab.always" =            true;
  # "userChrome.padding.drag_space" =                  true;
  # "userChrome.padding.drag_space.maximized" =        true;

  "userChrome.padding.toolbar_button.compact" = true;
  # FIXME: not respected?
  "userChrome.padding.menu_compact" = false;
  "userChrome.padding.bookmark_menu.compact" = false;
  "userChrome.padding.urlView_expanding" = true;
  "userChrome.padding.urlView_result" = true;
  "userChrome.padding.panel_header" = false;

  # "userChrome.urlbar.iconbox_with_separator" =       true;

  # "userChrome.urlView.as_commandbar" =               true;
  # "userChrome.urlView.full_width_padding" =          true;
  "userChrome.urlView.always_show_page_actions" = false;
  # "userChrome.urlView.move_icon_to_left" =           true;
  # "userChrome.urlView.go_button_when_typing" =       true;
  "userChrome.urlView.focus_item_border" = true;

  "userChrome.tabbar.as_titlebar" = false; # <- safari-like. not recommended.
  "userChrome.tabbar.fill_width" = false;
  # "userChrome.tabbar.multi_row" =                    true;
  # "userChrome.tabbar.unscroll" =                     true;
  # "userChrome.tabbar.on_bottom" =                    true;
  # "userChrome.tabbar.on_bottom.above_bookmark" =     true; # Need on_bottom
  # "userChrome.tabbar.on_bottom.menubar_on_top" =     true; # Need on_bottom
  # "userChrome.tabbar.on_bottom.hidden_single_tab" =  true; # Need on_bottom
  "userChrome.tabbar.one_liner" = false;
  "userChrome.tabbar.one_liner.combine_navbar" = false; # Need one_liner
  "userChrome.tabbar.one_liner.tabbar_first" = false; # Need one_liner
  "userChrome.tabbar.one_liner.responsive" = false; # Need one_liner

  "userChrome.tab.blue_accent" = true; # Lepton v8.5.1+
  # "userChrome.tab.bottom_rounded_corner.all" =       true;
  # "userChrome.tab.bottom_rounded_corner.australis" = true;
  # "userChrome.tab.bottom_rounded_corner.edge" =      true;
  # "userChrome.tab.bottom_rounded_corner.chrome" =    true;
  # "userChrome.tab.bottom_rounded_corner.chrome_legacy" = true;
  # "userChrome.tab.bottom_rounded_corner.wave" =      true;
  # "userChrome.tab.always_show_tab_icon" =            true;
  "userChrome.tab.close_button_at_pinned" = false; # <- enabled is rather unusable
  # "userChrome.tab.close_button_at_pinned.always" =   true;
  "userChrome.tab.close_button_at_pinned.background" = false;
  # "userChrome.tab.close_button_at_hover.always" =    true; # Need close_button_at_hover
  # "userChrome.tab.close_button_at_hover.with_selected" = true;  # Need close_button_at_hover
  "userChrome.tab.selected_bold" = true;
  # "userChrome.tab.sound_show_label" =                true; # Need remove sound_hide_label

  # "userChrome.navbar.as_sidebar" =                   true;

  "userChrome.bookmarkbar.multi_row" = false;

  # "userChrome.findbar.floating_on_top" =             true;

  # "userChrome.panel.remove_strip" =                  true;
  # "userChrome.panel.full_width_separator" =          true;
  # "userChrome.panel.full_width_padding" =            true;

  # NOTE: Not recommended with TST.
  # TODO: how to make this depend on extension?
  "userChrome.sidebar.overlap" = lib.mkDefault true;

  # "userChrome.icon.disabled" =                       true;
  # "userChrome.icon.account_image_to_right" =         true;
  # "userChrome.icon.account_label_to_right" =         true;
  "userChrome.icon.menu.full" = true;
  # "userChrome.icon.global_menu.mac" =                true;

  # -- userContent -------------------------------------------------------------

  # "userContent.player.ui.twoline" =                  true;

  # "userContent.newTab.hidden_logo" =                 true;

  "userContent.page.proton_color.dark_blue_accent" = true;
  "userContent.page.proton_color.system_accent" = true;
  "userContent.page.monospace" = true;
  # TODO: does this force dark mode or respect system?
  "userContent.page.dark_mode.pdf" = true;

  # == Theme Default Settings ===================================================

  # -- userChrome --------------------------------------------------------------

  "userChrome.compatibility.theme" = true;
  "userChrome.compatibility.os" = true;

  "userChrome.theme.built_in_contrast" = true;
  "userChrome.theme.system_default" = true;
  "userChrome.theme.proton_color" = true;
  "userChrome.theme.proton_chrome" = true; # Need proton_color
  "userChrome.theme.fully_color" = true; # Need proton_color
  "userChrome.theme.fully_dark" = true; # Need proton_color

  "userChrome.theme.transparent.frame" = lib.mkDefault false;
  "userChrome.theme.transparent.menu" = lib.mkDefault false;
  "userChrome.theme.transparent.panel" = lib.mkDefault false;

  "userChrome.decoration.cursor" = true;
  "userChrome.decoration.field_border" = true;
  "userChrome.decoration.download_panel" = true;
  "userChrome.decoration.animate" = true;

  "userChrome.padding.tabbar_width" = true;
  "userChrome.padding.tabbar_height" = true;
  "userChrome.padding.toolbar_button" = true;
  "userChrome.padding.navbar_width" = true;
  "userChrome.padding.urlbar" = true;
  "userChrome.padding.bookmarkbar" = true;
  "userChrome.padding.infobar" = true;
  "userChrome.padding.menu" = true;
  "userChrome.padding.bookmark_menu" = true;
  "userChrome.padding.global_menubar" = true;
  "userChrome.padding.panel" = true;
  "userChrome.padding.popup_panel" = true;

  "userChrome.tab.multi_selected" = true;
  "userChrome.tab.unloaded" = true;
  "userChrome.tab.letters_cleary" = true;
  "userChrome.tab.close_button_at_hover" = true;
  "userChrome.tab.sound_hide_label" = lib.mkDefault true;
  "userChrome.tab.sound_with_favicons" = true;
  "userChrome.tab.sound_with_favicons.on_center" = lib.mkDefault false;
  "userChrome.tab.pip" = true;
  "userChrome.tab.container" = true;
  "userChrome.tab.container.on_top" = true;
  "userChrome.tab.crashed" = true;

  "userChrome.fullscreen.overlap" = true;
  "userChrome.fullscreen.show_bookmarkbar" = true;

  "userChrome.icon.library" = true;
  "userChrome.icon.panel" = true;
  "userChrome.icon.menu" = true;
  "userChrome.icon.context_menu" = true;
  "userChrome.icon.global_menu" = true;
  "userChrome.icon.global_menubar" = true;

  # -- userContent -------------------------------------------------------------

  "userContent.player.ui" = true;
  "userContent.player.icon" = true;
  "userContent.player.noaudio" = true;
  "userContent.player.size" = true;
  "userContent.player.click_to_play" = true;
  "userContent.player.animate" = true;

  "userContent.newTab.full_icon" = true;
  "userContent.newTab.animate" = true;
  "userContent.newTab.pocket_to_last" = true;
  "userContent.newTab.searchbar" = true;

  "userContent.page.field_border" = true;
  "userContent.page.illustration" = true;
  "userContent.page.proton_color" = true;
  "userContent.page.dark_mode" = true; # Need proton_color
  "userContent.page.proton" = true; # Need proton_color

  # ** Useful Options ***********************************************************

  # Integrated calculator at urlbar
  "browser.urlbar.suggest.calculator" = true;

  # Integrated unit convertor at urlbar
  "browser.urlbar.unitConversion.enabled" = true;

  # Draw in Titlebar
  "browser.tabs.drawInTitlebar" = true;
  "browser.tabs.inTitlebar" = 1; # Nightly, 96 Above
}
