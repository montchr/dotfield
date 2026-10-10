{
  users.cdom.aspects.noctalia.home =
    { config, ... }:
    {
      programs.noctalia.settings.idle = {
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
            timeout = 600;
          };
          lock-and-suspend = {
            action = "lock_and_suspend";
            enabled = false;
            timeout = 900;
          };
          screen-off = {
            action = "screen_off";
            enabled = false;
            timeout = 660;
          };
        };
      };
    };
}
