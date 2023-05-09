{lib}: {config, ...}: let
  # inherit (inputs.apparat.lib) mkOpt';
  inherit (lib) mkForce mkOption types;

  cfg = config.lepton-ui;

  required = {
  };

  themes = {
    lepton = {
      userChrome.tab.connect_to_window = true; # Original, Photon
      userChrome.tab.color_like_toolbar = true; # Original, Photon
      userChrome.tab.lepton_like_padding = true; # Original
      userChrome.tab.newtab_button_like_tab = true; # Original
      userChrome.icon.panel_full = true; # Original, Proton
      userChrome.tab.box_shadow = true;
      userChrome.tab.bottom_rounded_corner = true;
    };

    photon = {
      userChrome.tab.photon_like_padding = true; # Photon
      userChrome.tab.connect_to_window = true; # Original, Photon
      userChrome.tab.color_like_toolbar = true; # Original, Photon
      userChrome.tab.newtab_button_smaller = true; # Photon
      userChrome.icon.panel_photon = true; # Photon
      userChrome.tab.photon_like_contextline = true;
      userChrome.rounding.square_tab = false;
    };

    proton = {
      userChrome.tab.newtab_button_proton = true; # Proton
      userChrome.icon.panel_full = true; # Original, Proton
    };
  };

  compat = {
    # userChrome.compatibility.accent_color =         true; # Firefox v103 Below
    # userChrome.compatibility.covered_header_image = true;
    # userChrome.compatibility.panel_cutoff =         true;
    # userChrome.compatibility.navbar_top_border =    true;
    # userChrome.compatibility.dynamic_separator =    true; # Need dynamic_separator

    # userChrome.compatibility.os.linux_non_native_titlebar_button = true;
  };

  uiDefaults = {
    general = {
      userChrome.compatibility.theme = true;
      userChrome.compatibility.os = true;
      animate = {
        userChrome.decoration.animate = true;
      };
    };

    optimizedContrast = {
      userChrome.theme.built_in_contrast = true;
    };
    followSystemAppearance = {
      userChrome.theme.system_default = true;
    };
    protonColor.enable = {
      userChrome.theme.proton_color = true;
      userChrome.theme.proton_chrome = true; # Need proton_color
      userChrome.theme.fully_color = true; # Need proton_color
      userChrome.theme.fully_dark = true; # Need proton_color
    };
  };

  userContent = {
    # Browser Theme Based Scheme - Will be activate 95 Above
    # layout.css.prefers-color-scheme.content-override = 3;
  };

  features = {
    autoHide = {
      enable = false;
      components = {
        sidebar = {
          userChrome.autohide.sidebar = features.autoHide.enable;
        };
      };
    };
    animation = {
      enable = true;
      components = {
        panel = {
          # NOTE: this is a double-negative option
          userChrome.decoration.disable_panel_animate = !features.animation.enable;
        };
        sidebar = {
          animate = {
            # NOTE: this is a double-negative option
            userChrome.decoration.disable_sidebar_animate = !features.animation.enable;
          };
        };
      };
    };
  };

  uiOptionals = {
    general = {
      animationTweaks = {
      };
    };
    font.monospace = {
      userChrome.theme.monospace = true;
    };
    color = {
      accent.blue = {
        userChrome.theme.proton_color.dark_blue_accent = true;
      };
    };
    panel = {
      animate.enable = {
        # NOTE: this is a double-negative option
        userChrome.decoration.disable_panel_animate = !features.animation.enable;
      };
      button.separator.enable = {
        userChrome.decoration.panel_button_separator = true;
      };
      arrow.enable = {
        userChrome.decoration.panel_arrow = true;
      };
    };
    sidebar = {
      animate.enable = {
        # NOTE: this is a double-negative option
        userChrome.decoration.disable_sidebar_animate = !features.animation.enable;
      };
      autoHide.enable = {
        userChrome.autohide.sidebar = true;
      };
    };
    tabBar = {
      autoHide.enable = {
        userChrome.autohide.tabbar = true;
      };
      separator.enable = {
        userChrome.tab.bar_separator = false;
      };
      tab = {
        separator.type = {
          dynamic = {
            userChrome.tab.dynamic_separator = true; # Original, Proton
          };
          static = {
            userChrome.tab.static_separator.selected_accent = false; # Just option
          };
        };
        autoHide = {
          enable = {
            userChrome.autohide.tab = false;
          };
          effects.opacity = {
            userChrome.autohide.tab.opacity = true;
          };
          effects.blur = {
            userChrome.autohide.tab.blur = true;
          };
        };
      };
    };
    navBar = {
      enable = {
      };
      autoHide.enable = {
        userChrome.autohide.navbar = true;
      };
      urlBar.autoExpand.enable = {
        userChrome.autohide.fill_urlbar = true;
      };
      backButton.autoHide.enable = {
        userChrome.autohide.back_button = true;
      };
      forwardButton.autoHide.enable = {
        userChrome.autohide.forward_button = true;
      };
      pageAction.autoHide = {
        # userChrome.autohide.page_action =                true;
      };
    };
    bookmarkBar.autoHide.enable = {
      userChrome.autohide.bookmarkbar = true;
    };
    toolbar.overlap = {
      enable = {
        userChrome.autohide.toolbar_overlap = true;
      };
      allowLayoutShift = {
        # userChrome.autohide.toolbar_overlap.allow_layout_shift = true;
      };
    };
  };

  # == Theme Custom Settings ====================================================
  # -- User Chrome --------------------------------------------------------------
  # userChrome.hidden.tab_icon =                     true;
  # userChrome.hidden.tab_icon.always =              true;
  # userChrome.hidden.tabbar =                       true;
  # userChrome.hidden.navbar =                       true;
  # userChrome.hidden.titlebar_container =           true;
  # userChrome.hidden.sidebar_header =               true;
  # userChrome.hidden.sidebar_header.vertical_tab_only = true;
  # userChrome.hidden.urlbar_iconbox =               true;
  # userChrome.hidden.urlbar_iconbox.label_only =    true;
  # userChrome.hidden.bookmarkbar_icon =             true;
  # userChrome.hidden.bookmarkbar_label =            true;
  # userChrome.hidden.disabled_menu =                true;
  # userChrome.centered.tab =                        true;
  # userChrome.centered.tab.label =                  true;
  # userChrome.centered.urlbar =                     true;
  # userChrome.centered.bookmarkbar =                true;
  # userChrome.counter.tab =                         true;
  # userChrome.counter.bookmark_menu =               true;
  # userChrome.combined.nav_button =                 true;
  # userChrome.combined.nav_button.home_button =     true;
  # userChrome.combined.urlbar.nav_button =          true;
  # userChrome.combined.urlbar.home_button =         true;
  # userChrome.combined.urlbar.reload_button =       true;
  # userChrome.combined.sub_button.none_background = true;
  # userChrome.combined.sub_button.as_normal =       true;
  # userChrome.rounding.square_button =              true;
  # userChrome.rounding.square_dialog =              true;
  # userChrome.rounding.square_panel =               true;
  # userChrome.rounding.square_panelitem =           true;
  # userChrome.rounding.square_menupopup =           true;
  # userChrome.rounding.square_menuitem =            true;
  # userChrome.rounding.square_infobox =             true;
  # userChrome.rounding.square_toolbar =             true;
  # userChrome.rounding.square_field =               true;
  # userChrome.rounding.square_urlView_item =        true;
  # userChrome.rounding.square_checklabel =          true;
  # userChrome.padding.first_tab =                   true;
  # userChrome.padding.first_tab.always =            true;
  # userChrome.padding.drag_space =                  true;
  # userChrome.padding.drag_space.maximized =        true;
  # userChrome.padding.toolbar_button.compact =      true;
  # userChrome.padding.menu_compact =                true;
  # userChrome.padding.bookmark_menu.compact =       true;
  # userChrome.padding.urlView_expanding =           true;
  # userChrome.padding.urlView_result =              true;
  # userChrome.padding.panel_header =                true;
  # userChrome.urlbar.iconbox_with_separator =       true;
  # userChrome.urlView.as_commandbar =               true;
  # userChrome.urlView.full_width_padding =          true;
  # userChrome.urlView.always_show_page_actions =    true;
  # userChrome.urlView.move_icon_to_left =           true;
  # userChrome.urlView.go_button_when_typing =       true;
  # userChrome.urlView.focus_item_border =           true;
  # userChrome.tabbar.as_titlebar =                  true;
  # userChrome.tabbar.fill_width =                   true;
  # userChrome.tabbar.multi_row =                    true;
  # userChrome.tabbar.unscroll =                     true;
  # userChrome.tabbar.on_bottom =                    true;
  # userChrome.tabbar.on_bottom.above_bookmark =     true; # Need on_bottom
  # userChrome.tabbar.on_bottom.menubar_on_top =     true; # Need on_bottom
  # userChrome.tabbar.on_bottom.hidden_single_tab =  true; # Need on_bottom
  # userChrome.tabbar.one_liner =                    true;
  # userChrome.tabbar.one_liner.combine_navbar =     true; # Need one_liner
  # userChrome.tabbar.one_liner.tabbar_first =       true; # Need one_liner
  # userChrome.tabbar.one_liner.responsive =         true; # Need one_liner
  # userChrome.tab.bottom_rounded_corner.all =       true;
  # userChrome.tab.bottom_rounded_corner.australis = true;
  # userChrome.tab.bottom_rounded_corner.edge =      true;
  # userChrome.tab.bottom_rounded_corner.chrome =    true;
  # userChrome.tab.bottom_rounded_corner.chrome_legacy = true;
  # userChrome.tab.bottom_rounded_corner.wave =      true;
  # userChrome.tab.always_show_tab_icon =            true;
  # userChrome.tab.close_button_at_pinned =          true;
  # userChrome.tab.close_button_at_pinned.always =   true;
  # userChrome.tab.close_button_at_pinned.background = true;
  # userChrome.tab.close_button_at_hover.always =    true; # Need close_button_at_hover
  # userChrome.tab.close_button_at_hover.with_selected = true;  # Need close_button_at_hover
  # userChrome.tab.sound_show_label =                true; # Need remove sound_hide_label
  # userChrome.navbar.as_sidebar =                   true;
  # userChrome.bookmarkbar.multi_row =               true;
  # userChrome.findbar.floating_on_top =             true;
  # userChrome.panel.remove_strip =                  true;
  # userChrome.panel.full_width_separator =          true;
  # userChrome.panel.full_width_padding =            true;
  # userChrome.sidebar.overlap =                     true;
  # userChrome.icon.disabled =                       true;
  # userChrome.icon.account_image_to_right =         true;
  # userChrome.icon.account_label_to_right =         true;
  # userChrome.icon.menu.full =                      true;
  # userChrome.icon.global_menu.mac =                true;
  # -- User Content -------------------------------------------------------------
  # userContent.player.ui.twoline =                  true;
  # userContent.newTab.hidden_logo =                 true;
  # userContent.page.proton_color.dark_blue_accent = true;
  # userContent.page.proton_color.system_accent =    true;
  # userContent.page.monospace =                     true;

  # == Theme Default Settings ===================================================
  # -- User Chrome --------------------------------------------------------------
  todoThemeDefaults = {
    # userChrome.theme.built_in_contrast = true;
    # userChrome.theme.system_default = true;
    # userChrome.theme.proton_color = true;
    # userChrome.theme.proton_chrome = true; # Need proton_color
    # userChrome.theme.fully_color = true; # Need proton_color
    # userChrome.theme.fully_dark = true; # Need proton_color

    userChrome.decoration.cursor = true;
    userChrome.decoration.field_border = true;
    userChrome.decoration.download_panel = true;

    userChrome.padding.tabbar_width = true;
    userChrome.padding.tabbar_height = true;
    userChrome.padding.toolbar_button = true;
    userChrome.padding.navbar_width = true;
    userChrome.padding.urlbar = true;
    userChrome.padding.bookmarkbar = true;
    userChrome.padding.infobar = true;
    userChrome.padding.menu = true;
    userChrome.padding.bookmark_menu = true;
    userChrome.padding.global_menubar = true;
    userChrome.padding.panel = true;
    userChrome.padding.popup_panel = true;

    userChrome.tab.multi_selected = true;
    userChrome.tab.unloaded = true;
    userChrome.tab.letters_cleary = true;
    userChrome.tab.close_button_at_hover = true;
    userChrome.tab.sound_hide_label = true;
    userChrome.tab.sound_with_favicons = true;
    userChrome.tab.pip = true;
    userChrome.tab.container = true;
    userChrome.tab.crashed = true;

    userChrome.fullscreen.overlap = true;
    userChrome.fullscreen.show_bookmarkbar = true;

    userChrome.icon.library = true;
    userChrome.icon.panel = true;
    userChrome.icon.menu = true;
    userChrome.icon.context_menu = true;
    userChrome.icon.global_menu = true;
    userChrome.icon.global_menubar = true;

    # -- User Content -------------------------------------------------------------
    userContent.player.ui = true;
    userContent.player.icon = true;
    userContent.player.noaudio = true;
    userContent.player.size = true;
    userContent.player.click_to_play = true;
    userContent.player.animate = true;

    userContent.newTab.full_icon = true;
    userContent.newTab.animate = true;
    userContent.newTab.pocket_to_last = true;
    userContent.newTab.searchbar = true;

    userContent.page.field_border = true;
    userContent.page.illustration = true;
    userContent.page.proton_color = true;
    userContent.page.dark_mode = true; # Need proton_color
    userContent.page.proton = true; # Need proton_color

    # Integrated calculator at urlbar
    browser.urlbar.suggest.calculator = true;

    # Integrated unit convertor at urlbar
    # browser.urlbar.unitConversion.enabled = true;

    # Draw in Titlebar
    # browser.tabs.drawInTitlebar = true;
    # browser.tabs.inTitlebar =        1; # Nightly, 96 Above
  };
in {
  options.lepton-ui = {
    enable = lib.mkEnableOption "Lepton UI";
    theme = {
      flavor = mkOption {
        type = types.enum ["lepton" "photon" "proton"];
        default = "lepton";
        description = ''
          User interface theme flavor.
        '';
      };
    };
    features = {
      autoHide = {
        enable = lib.mkEnableOption "UI component autohiding.";
        components = {
          sidebar = {
            userChrome.autohide.sidebar = features.autoHide.enable;
          };
        };
      };
      animation = {
        enable = lib.mkEnableOption "UI component animation.";
        components = {
          panel = {
            # NOTE: this is a double-negative option
            userChrome.decoration.disable_panel_animate = !features.animation.enable;
          };
          sidebar = {
            animate = {
              # NOTE: this is a double-negative option
              userChrome.decoration.disable_sidebar_animate = !features.animation.enable;
            };
          };
        };
      };
    };
    settings = lib.mkOption {
      # TODO: type for user pref
      type = with types; attrsOf unspecified;
    };
  };
  config = lib.mkIf cfg.enable (lib.mkMerge [
    (themes.${cfg.theme.flavor})
    {
      settings = {
        # Enable support for user stylesheet customisations.
        toolkit.legacyUserProfileCustomizations.stylesheets = mkForce true;

        # Compat: enable Proton UI for ESR (removed in FF97)
        browser.proton.enabled = true;

        # SVG fill color support
        svg.context-properties.content.enabled = true;

        # FF88+: CSS color mix
        layout.css.color-mix.enabled = true;

        # FF88+: CSS blur filter
        layout.css.backdrop-filter.enabled = true;

        # FF89+: Restore compact UI mode
        browser.compactmode.show = true;

        # FF89+: Enable `about:home` search bar
        browser.newtabpage.activity-stream.improvesearch.handoffToAwesomebar = false;

        # FF103+: Enable CSS `:has()` selector
        layout.css.has-selector.enabled = true;
      };
    }
  ]);
}
