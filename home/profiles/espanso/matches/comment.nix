{
  services.espanso = {
    global_vars = [
      {
        name = "clip";
        type = "clipboard";
      }
    ];

    matches = [
      {
        replace = "# <{{clipped}}>";
        trigger = "# <";
        vars = [
          {
            depends_on = [ "clip" ];
            name = "clipped";
            params = {
              cmd = "echo \"$ESPANSO_CLIP\" \\\n| grep '^https://' || [[ $? == 1 ]]\n";
            };
            type = "shell";
          }
        ];
      }
      {
        replace = ";; <{{clipped}}>";
        trigger = ";; <";
        vars = [
          {
            depends_on = [ "clip" ];
            name = "clipped";
            params = {
              cmd = "echo \"$ESPANSO_CLIP\" \\\n| grep '^https://' || [[ $? == 1 ]]\n";
            };
            type = "shell";
          }
        ];
      }
      {
        replace = "' :: <{{clipped}}>";
        trigger = "' :: <";
        vars = [
          {
            depends_on = [ "clip" ];
            name = "clipped";
            params = {
              cmd = "echo \"$ESPANSO_CLIP\" \\\n| grep '^https://' || [[ $? == 1 ]]\n";
            };
            type = "shell";
          }
        ];
      }
      {
        replace = "# SPDX-FileCopyrightText: {{year}} {{form.author}}\n# SPDX-License-Identifier: {{form.license}}";
        trigger = ";#spdx";
        vars = [
          {
            name = "form";
            params = {
              fields = {
                author = {
                  default = "Chris Montgomery <chmont@proton.me>";
                };
                license = {
                  default = "GPL-3.0-or-later";
                  type = "choice";
                  values = [
                    "GPL-3.0-or-later"
                    "MIT"
                    "Apache-2.0"
                    "MPL-2.0"
                    "BSD-3-Clause"
                  ];
                };
              };
              layout = "author: [[author]]\nlicense: [[license]]\n";
            };
            type = "form";
          }
          {
            name = "year";
            params = {
              format = "%Y";
            };
            type = "date";
          }
        ];
      }
    ];
  };
}
