final: prev: {
  inherit (final.beetsPackages) beets beets-unstable;

  beetsPackages = prev.beetsPackages.extend (
    bpfinal: bpprev: {
      beets = bpfinal.beets-unstable;
      beets-minimal = bpfinal.beets-unstable.override { disableAllPlugins = true; };
      beets-unstable = bpprev.beets-unstable.overrideAttrs (o: {
        patches = o.patches ++ [
          # <https://github.com/beetbox/beets/issues/5473>
          # <https://github.com/beetbox/beets/pull/5415>
          ../packages/beets/patches/5415-Discogs-plugin-type-error.patch
        ];
      });
    }
  );
}
