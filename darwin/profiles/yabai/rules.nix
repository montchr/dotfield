let
  ignore = {
    manage = false;
    sticky = false;
  };
  opaque = {opacity = "1.0";};
in [
  ["1Password" ignore]
  ["Alfred Preferences" ignore]
  ["AppCleaner" ignore]
  ["Fanatastical Helper" ignore]
  ["Stickies" ignore]

  # Ventura changed the name to System Settings.
  ["^System (Settings|Preferences)$" ignore]

  # JetBrains apps do not work well with yabai due to many popup dialogs.
  # TODO: It may be possible to target just the dialogs.
  ["PhpStorm" ignore]

  # Prevent tiny file copy dialogs from claiming a space partition.
  ["^Finder$" (ignore // {title = "Copy";})]

  ["Microsoft Teams" opaque]
  ["zoom.us" opaque]

  ## Emacs

  [
    "Emacs"
    {
      title = "doom-capture";
      manage = false;
      grid = "3:3:1:1:1:1";
      label = "[Emacs]: Float and center the doom capture window";
    }
  ]

  [
    "Emacs"
    {
      title = ".*Minibuf.*";
      manage = false;
      label = "[Emacs]: Float minibuffer";
    }
  ]
]
