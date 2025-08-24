{
  dotfield.users.cdom.aspects.gnome-desktop.home =
    { config, ... }:
    let
      inherit (config.lib.hm.gvariant) mkUint32;
      lib' = config.lib;
    in
    {
      dconf.settings = {
        "org/gnome/desktop/interface" = {
          clock-show-seconds = false;
          clock-show-weekday = true;
        };

        "org/gnome/desktop/media-handling" = {
          autorun-never = true;
        };

        "org/gnome/desktop/search-providers" = {
          sort-order = [
            "org.gnome.Nautilus.desktop"
            "org.gnome.Documents.desktop"
          ];
        };

        "org/gnome/desktop/session" = {
          # TODO: longer for home theater, which are the primary gnome users
          idle-delay = (mkUint32 600);
        };

        "org/gnome/shell/app-switcher" = {
          current-workspace-only = false;
        };

        "org/gnome/mutter" = {
          # Modals might get stuck without this setting.
          attach-modal-dialogs = true;

          dynamic-workspaces = true;
          edge-tiling = true;
        };

        "org/gnome/desktop/wm/preferences" = {
          auto-raise = true;
          button-layout = "appmenu:minimize,close";
          focus-mode = "click";
          num-workspaces = 2;
        };

        "org/gnome/desktop/privacy" = {
          old-files-age = lib'.hm.gvariant.mkUint32 30;
          recent-files-max-age = -1;
        };

        "org/gnome/desktop/peripherals/keyboard" = {
          repeat = true;
        };

      };
    };
}
