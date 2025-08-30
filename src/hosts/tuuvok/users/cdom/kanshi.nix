{
  hosts.nixos.tuuvok.users.cdom.configuration = {
    services.kanshi.settings = [
      {
        output.criteria = "eDP-1";
        output.scale = 2.0;
      }
      {
        output.criteria = "LG Electronics LG Ultra HD 0x000668B9";
        output.scale = 2.0;
        output.mode = "3840x2160";
      }
      {
        output.criteria = "LG Electronics LG ULTRAGEAR 107NTBKA5869";
        output.scale = 1.0;
        output.mode = "2560x1440";
      }
      {
        profile.name = "undocked";
        profile.outputs = [
          {
            criteria = "eDP-1";
            status = "enable";
            scale = 2.0;
          }
        ];
      }
      {
        profile.name = "workdock";
        profile.outputs = [
          {
            criteria = "eDP-1";
            status = "disable";
            scale = 2.0;
          }
          {
            criteria = "LG Electronics LG Ultra HD 0x000668B9";
            status = "enable";
            position = "0,0";
            scale = 2.0;
          }
        ];
      }
      {
        profile.name = "homedock";
        profile.outputs = [
          {
            criteria = "eDP-1";
            status = "disable";
          }
          {
            criteria = "LG Electronics LG ULTRAGEAR 107NTBKA5869";
            status = "enable";
            position = "0,0";
          }
        ];
      }
    ];
  };
}
