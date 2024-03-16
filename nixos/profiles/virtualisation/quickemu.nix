_: {
  # services.samba.enable = true;
  # services.samba.shares = [];
  environment.systemPackages = [
    # FIXME: oh wait we must never ever build a custom qemu! that will take... months...?
    #    (pkgs.quickemu.override {
    #      qemu = pkgs.qemu.override {
    #        inherit pipewireSupport;
    #
    #        # Fix issues with offset cursor on Wayland hosts.
    #        # <https://gitlab.gnome.org/GNOME/mutter/-/merge_requests/2040>
    #        # <https://old.reddit.com/r/kde/comments/tbr5ey/annoying_mouse_cursor_misalignment_in_plasma/>
    #        openGLSupport = true;
    #
    #        # FIXME: conflicts with nixos service: <https://github.com/quickemu-project/quickemu/issues/722>
    #        #
    #        #        should throw an error if:
    #        #        1) the service is enabled and
    #        #        2) this attribute is set to true (the default value!)
    #        #
    #        #        To accomplish this check I'm guessing would need to be handled in
    #        #        a NixOS module for quickemu, which does not yet exist. smbdSupport =
    #        #        !config.services.samba.enable;
    #        smbdSupport = true;
    #      };
    #    })
  ];
}
