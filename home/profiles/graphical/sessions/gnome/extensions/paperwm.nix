{
  dconf.settings = {
    "org/gnome/shell/extensions/paperwm" = {
      default-focus-mode = 1;
      horizontal-margin = 16;
      open-window-position = 0;
      # FIXME: this setting might cause major display manager crash!
      restore-attach-modal-dialogs = "";
      restore-edge-tiling = "";
      restore-keybinds = ''
        {}
      '';
      restore-workspaces-only-on-primary = "";
      selection-border-radius-top = 8;
      selection-border-size = 6;
      show-workspace-indicator = true;
      vertical-margin = 16;
      vertical-margin-bottom = 16;
      window-gap = 16;
    };

    "org/gnome/shell/extensions/paperwm/keybindings" = {
      move-down = [
        "<Control><Super>Down"
        "<Shift><Super>s"
      ];
      move-left = [
        "<Control><Super>comma"
        "<Shift><Super>comma"
        "<Control><Super>Left"
        "<Shift><Super>a"
      ];
      move-right = [
        "<Control><Super>period"
        "<Shift><Super>period"
        "<Control><Super>Right"
        "<Shift><Super>d"
      ];
      move-up = [
        "<Control><Super>Up"
        "<Shift><Super>w"
      ];
      switch-down = [
        "<Super>Down"
        "<Super>s"
      ];
      switch-left = [
        "<Super>Left"
        "<Super>a"
      ];
      switch-next = [ "<Super>period" ];
      switch-previous = [ "<Super>comma" ];
      switch-right = [
        "<Super>Right"
        "<Super>d"
      ];
      switch-up = [
        "<Super>Up"
        "<Super>w"
      ];
    };

  };
}
