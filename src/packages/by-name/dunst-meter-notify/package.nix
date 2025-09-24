{
  bc,
  brightnessctl,
  dunst,
  gawk,
  wireplumber,
  writeShellApplication,
}:
writeShellApplication {
  name = "dunst-meter-notify";
  runtimeInputs = [
    bc
    brightnessctl
    dunst
    gawk
    wireplumber
  ];
  text = builtins.readFile ./script.sh;
}
