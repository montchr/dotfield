{
  dotfield.features."desktop-environments/wayland-wm".home = {
    services.dunst = {
      enable = true;
      settings = {
        global = {
          width = 400;
          height = 60;
          offset = "30x50";
          origin = "top-center";
          transparency = 10;
        };
        urgency_normal = {
          timeout = 10;
        };
      };
    };
  };
}
