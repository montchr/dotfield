{
  matches = [
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
      replace = "×";
      triggers = [
        ";s;mult"
        ":ssmultiply"
      ];
    }
    {
      replace = "∅";
      triggers = [
        ";s;null"
        ":ssnull"
        ":ssnil"
      ];
    }
    {
      replace = "¯\\_(ツ)_/¯";
      triggers = [
        ";s;hrug"
        ";s;shrug"
        ":emoshrug"
      ];
    }
  ];
}
