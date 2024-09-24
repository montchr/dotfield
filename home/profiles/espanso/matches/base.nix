{
  services.espanso.matches = [
    {
      replace = "⌘";
      trigger = ";s;cmd";
    }
    {
      replace = "⇧";
      trigger = ";s;shift";
    }
    {
      replace = "⌃";
      trigger = ";s;ctrl";
    }
    {
      replace = "⌥";
      trigger = ";s;opt";
    }
    {
      replace = "⎋";
      trigger = ";s;esc";
    }
    {
      replace = "←";
      triggers = [
        ";s;a;ll"
        ";s;a;ww"
      ];
    }
    {
      replace = "→";
      triggers = [
        ";s;a;rr"
        ";s;a;ee"
      ];
    }
    {
      replace = "⇒";
      triggers = [ ";s;aa;rr" ];
    }
    {
      replace = "⇐";
      triggers = [ ";s;aa;ll" ];
    }
    {
      replace = "↑";
      triggers = [
        ";s;a;uu"
        ";s;a;nn"
      ];
    }
    {
      replace = "↓";
      triggers = [
        ";s;a;dd"
        ";s;a;ss"
      ];
    }
    {
      replace = "↖";
      triggers = [
        ";s;a;ul"
        ";s;a;nw"
      ];
    }
    {
      replace = "↗";
      triggers = [
        ";s;a;ur"
        ";s;a;ne"
      ];
    }
    {
      replace = "↘";
      triggers = [
        ";s;a;dr"
        ";s;a;se"
      ];
    }
    {
      replace = "↙";
      triggers = [
        ";s;a;dl"
        ";s;a;sw"
      ];
    }
    {
      replace = "×";
      trigger = ";s;mult";
    }
    {
      replace = "∅";
      trigger = ";s;null";
    }
    {
      replace = "¯\\_(ツ)_/¯";
      trigger = ";s;hrug";
    }
  ];
}
