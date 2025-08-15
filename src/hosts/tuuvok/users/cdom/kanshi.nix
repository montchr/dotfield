flake:
let
  inherit (flake.config.dotfield) meta;
in
{
  dotfield.hosts.nixos.tuuvok.users.cdom.home = {
    services.kanshi.settings = [
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
            inherit (meta.displays."lg/27UD88-W") scale;
            criteria = meta.displays."lg/27UD88-W".deviceName;
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
            inherit (meta.displays."lg/27GL850-B") scale;
            criteria = meta.displays."lg/27GL850-B".deviceName;
            status = "enable";
            position = "0,0";
          }
        ];
      }
    ];
  };
}
