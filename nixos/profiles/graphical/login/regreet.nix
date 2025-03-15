{
  users.extraUsers.greeter = {
    home = "/tmp/greeter-home";
    createHome = true;
  };

  programs.regreet = {
    enable = true;
    settings = {
      appearance = {
        greeting_msg = "Hello Word!";
      };
      commands = {
        reboot = [
          "systemctl"
          "reboot"
        ];
        poweroff = [
          "systemctl"
          "poweroff"
        ];
      };
      widget.clock = {
        format = "%a %T";
        label_width = 150;
      };
    };
  };
}
