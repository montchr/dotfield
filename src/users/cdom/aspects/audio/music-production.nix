{
  dotfield.users.cdom.aspects.music-production.home =
    { pkgs, ... }:
    {
      home.packages = with pkgs; [
        ardour
        carla
        dexed
        # XXX: broken as of 2025-04-18
        # mixxx
        puredata
        qsynth
        renoise
        samplv1
        vcv-rack
      ];
    };
}
