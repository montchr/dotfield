{
  global_vars = [
    {
      name = "date";
      params = {
        format = "%F";
      };
      type = "date";
    }
    {
      name = "time";
      params = {
        format = "%T";
      };
      type = "date";
    }
  ];

  matches = [
    {
      replace = "{{date}}";
      trigger = ";d;dd";
    }
    {
      replace = "{{dateFriendly}}";
      trigger = ";d;df";
      vars = [
        {
          name = "dateFriendly";
          params = {
            format = "%B %e, %Y";
          };
          type = "date";
        }
      ];
    }
    {
      replace = "{{time}}";
      trigger = ";d;tt";
    }
    {
      replace = "{{date}} {{time}}";
      trigger = ";d;dt";
    }
    {
      replace = "{{id}}";
      trigger = ";d;id";
      vars = [
        {
          name = "id";
          params = {
            format = "%Y%m%d%H%M%S";
          };
          type = "date";
        }
      ];
    }
  ];
}
