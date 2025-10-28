{
  users.cdom.aspects.workstation.home = {
    services.kanshi.settings = [
      {
        profile.name = "undocked";
        profile.outputs = [
          {
            criteria = "eDP-1";
            status = "enable";
          }
        ];
      }
      {
        profile.name = "workdock";
        profile.outputs = [
          {
            criteria = "eDP-1";
            status = "disable";
          }
          {
            criteria = "LG Electronics LG Ultra HD 0x000668B9";
            status = "enable";
            position = "0,0";
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
