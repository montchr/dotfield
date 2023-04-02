{
  l,
  lib,
  ...
}: let
  inherit (lib.booleans) boolToOnOffString;

  formatValue = v:
    if l.isBool v
    then boolToOnOffString v
    else l.toString v;

  toArgsString = args:
    l.concatStringsSep " " (l.mapAttrsToList
      (k: v: "${k}='${formatValue v}'")
      args);

  mkSetting = name: value: ''
    yabai -m config ${name} ${formatValue value}
  '';

  mkRule = app: args: ''
    yabai -m rule --add app='${app}' ${toArgsString args}
  '';

  mkSignal = event: action: args: ''
    yabai -m signal --add event='${event}' action='${action}' ${toArgsString args}
  '';
in {
  inherit toArgsString mkRule mkSetting mkSignal;

  mkSignal' = e: a: mkSignal e a {};

  /*
  @partial
  */
  mkRules = l.concatMapStrings (v: mkRule (l.head v) (l.last v));

  /*
  @partial
  */
  mkSignals = l.concatMapStrings mkSignal;

  toYabaiConfig = opts: l.concatLines (l.mapAttrsToList mkSetting opts);
}
