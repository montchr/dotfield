{
  services.espanso.matches.crypt.matches = [
    {
      trigger = ";;uuid";
      replace = "{{output}}";
      vars = [
        {
          name = "output";
          type = "shell";
          params = {
            cmd = "uuidgen -r";
          };
        }
      ];
    }
    {
      trigger = ";e;b64u";
      replace = "{{output}}";
      vars = [
        {
          name = "clip";
          type = "clipboard";
        }
        {
          name = "output";
          type = "shell";
          params = {
            cmd = "echo -n \"{{clip}}\" | basenc --base64url";
          };
        }
      ];
    }
    {
      trigger = ";d;b64u";
      replace = "{{output}}";
      vars = [
        {
          name = "clip";
          type = "clipboard";
        }
        {
          name = "output";
          type = "shell";
          params = {
            cmd = "echo -n \"{{clip}}\" | basenc --decode --base64url";
          };
        }
      ];
    }
  ];
}
