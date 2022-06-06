{
  settings = {
    required = {
      "browser.compactmode.show" = true;
      "browser.newtabpage.activity-stream.improvesearch.handoffToAwesomebar" = false;

      # This setting has been removed in Firefox 97 but retained for ESR compatibility.
      # "browser.proton.enabled" = true;

      "browser.proton.places-tooltip.enabled" = true;
      "layout.css.color-mix.enabled" = true;
      "layout.css.backdrop-filter.enabled" = true;

      # https://developer.mozilla.org/en-US/docs/Web/CSS/-moz-context-properties
      "svg.context-properties.content.enabled" = true;

      "toolkit.legacyUserProfileCustomizations.stylesheets" = true;
    };

    # theme.common = {
    #   # "userChrome.tab.static_separator.selected_accent" = true; # Optional enhancement
    #   "userChrome.icon.panel_sparse" = true; # Optional enhancement
    # };

    theme.lepton = {
      "userChrome.tab.connect_to_window" = true; # Original, Photon
      "userChrome.tab.color_like_toolbar" = true; # Original, Photon
      "userChrome.tab.lepton_like_padding" = true; # Original
      # FIXME: typo?
      "userChrome.tab.dynamic_separtor" = true; # Original, Proton
      "userChrome.tab.newtab_button_like_tab" = true; # Original
      "userChrome.icon.panel_full" = true; # Original, Proton
      "userChrome.tab.box_shadow" = true;
      "userChrome.tab.bottom_rounded_corner" = true;
      # TODO: describe values/options
      "layout.css.prefers-color-scheme.content-override" = 3;
    };

    theme.photon = {
      "userChrome.tab.connect_to_window" = true; # Original, Photon
      "userChrome.tab.color_like_toolbar" = true; # Original, Photon
      "userChrome.tab.photon_like_padding" = true; # Photon
      "userChrome.tab.static_separator" = true; # Photon
      "userChrome.tab.newtab_button_smaller" = true; # Photon
      "userChrome.icon.panel_photon" = true; # Photon
      "userChrome.tab.photon_like_contextline" = true;
      "userChrome.rounding.square_tab" = true;
    };

    theme.proton = {
      # TODO: typo?
      "userChrome.tab.dynamic_separtor" = true; # Original, Proton
      "userChrome.tab.newtab_button_proton" = true; # Proton
      "userChrome.icon.panel_full" = true; # Original, Proton
    };

    compat = {
      # "userChrome.compatibility.covered_header_image" = true;
      # "userChrome.compatibility.panel_cutoff" = true;
      # "userChrome.compatibility.navbar_top_border" = true;
      # "userChrome.compatibility.dynamic_separator" = true; # Need dynamic_seperator

      # "userChrome.compatiblity.os.linux_non_native_titlebar_button" = true;
    };

    recommended = {
      "userChrome.compatibility.theme" = true;
      "userChrome.compatibility.os" = true;

      "userChrome.theme.built_in_contrast" = true;
      "userChrome.theme.system_default" = true;
      "userChrome.theme.proton_color" = true;
      "userChrome.theme.proton_chrome" = true; # Need proton_color
      "userChrome.theme.fully_color" = true; # Need proton_color
      "userChrome.theme.fully_dark" = true; # Need proton_color

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
      # "userChrome.tab.sound_hide_label" = true;
      "userChrome.tab.sound_with_favicons" = true;
      "userChrome.tab.pip" = true;
      "userChrome.tab.container" = true;
      "userChrome.tab.crashed" = true;

      "userChrome.fullscreen.overlap" = true;
      "userChrome.fullscreen.show_bookmarkbar" = true;

      "userChrome.icon.library" = true;
      "userChrome.icon.panel" = true;
      "userChrome.icon.menu" = true;
      "userChrome.icon.context_menu" = true;
      "userChrome.icon.global_menu" = true;
      "userChrome.icon.global_menubar" = true;

      ## User Content

      "userContent.player.ui" = true;
      "userContent.player.icon" = true;
      "userContent.player.noaudio" = true;
      "userContent.player.size" = true;
      "userContent.player.click_to_play" = true;
      "userContent.player.animate" = true;

      "userContent.newTab.field_border" = true;
      "userContent.newTab.full_icon" = true;
      "userContent.newTab.animate" = true;
      "userContent.newTab.pocket_to_last" = true;
      "userContent.newTab.searchbar" = true;

      "userContent.page.illustration" = true;
      "userContent.page.proton_color" = true;
      "userContent.page.dark_mode" = true; # Need proton_color
      "userContent.page.proton" = true; # Need proton_color
    };

    optional = {
      ## User Chrome

      "userChrome.decoration.disable_panel_animate" = true;
      "userChrome.decoration.disable_sidebar_animate" = true;

      # "userChrome.theme.proton_color.dark_blue_accent" = true;

      "userChrome.rounding.square_button" = true;
      "userChrome.rounding.square_panel" = true;
      "userChrome.rounding.square_panelitem" = true;
      "userChrome.rounding.square_menupopup" = true;
      "userChrome.rounding.square_menuitem" = true;
      "userChrome.rounding.square_field" = true;
      "userChrome.rounding.square_checklabel" = true;

      "userChrome.padding.first_tab" = true;
      "userChrome.padding.drag_space" = true;
      "userChrome.padding.drag_space.maximized" = true;

      "userChrome.padding.menu_compact" = true;
      "userChrome.padding.bookmark_menu.compact" = true;
      "userChrome.padding.urlView_expanding" = true;
      "userChrome.padding.urlView_result" = true;
      "userChrome.padding.panel_header" = true;

      # "userChrome.urlView.move_icon_to_left" = true;
      # "userChrome.urlView.go_button_when_typing" = true;
      # "userChrome.urlView.always_show_page_actions" = true;

      # "userChrome.tab.on_bottom" = true;
      # "userChrome.tab.on_bottom.above_bookmark" = true; # Need on_bottom
      # "userChrome.tab.on_bottom.menubar_on_top" = true; # Need on_bottom
      # "userChrome.tab.always_show_tab_icon" = true;
      # "userChrome.tab.close_button_at_pinned" = true;
      # "userChrome.tab.close_button_at_pinned.always" = true;
      # "userChrome.tab.close_button_at_pinned.background" = true;
      "userChrome.tab.close_button_at_hover.always" = true; # Need close_button_at_hover
      "userChrome.tab.sound_show_label" = true; # Need remove sound_hide_label
      # "userChrome.tab.centered_label" = true;

      # "userChrome.panel.remove_strip" = true;
      # "userChrome.panel.full_width_separator" = true;
      # "userChrome.panel.full_width_padding" = true;

      # "userChrome.icon.account_image_to_right" = true;
      # "userChrome.icon.account_label_to_right" = true;

      ## User Content

      # "userContent.player.ui.twoline" = true;

      # "userContent.page.proton_color.dark_blue_accent" = true;
      "userContent.page.proton_color.system_accent" = true;
    };
  };
}
