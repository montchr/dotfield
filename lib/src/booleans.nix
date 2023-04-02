_: {
  /*
  Convert a boolean to a "yes" or "no" string.

  Commonly useful in program configuration files.

  Type: boolToOnOffString :: boolean -> string
  */
  boolToOnOffString = value:
    if value
    then "on"
    else "off";

  /*
  Convert a boolean to a "yes" or "no" string.

  Commonly useful in program configuration files.

  Type: boolToYesNoString :: boolean -> string
  */
  boolToYesNoString = value:
    if value
    then "yes"
    else "no";
}
