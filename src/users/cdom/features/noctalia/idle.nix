{
  users.cdom.aspects.noctalia.home =
    { config, ... }:
    let
      cfg = config.programs.noctalia;
    in
    {
      programs.noctalia.settings.idle =
        let
          self = cfg.settings.idle;
        in
        {
          pre_action_fade_seconds = 10;
          behavior_order = [
            "lock"
            "screen-off"
            "lock-and-suspend"
          ];
          behavior = {
            lock = {
              action = "lock";
              enabled = true;
              timeout = 60 * 20;
            };
            screen-off = {
              action = "screen_off";
              enabled = false;
              timeout = self.behavior.lock.timeout + 2;
            };
            # lock-and-suspend = {
            #   action = "lock_and_suspend";
            #   enabled = false;
            #   timeout = 900;
            # };
          };
        };
    };
}
