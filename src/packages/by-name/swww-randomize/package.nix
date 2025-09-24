{
  coreutils,
  swww,
  writeShellApplication,
}:
writeShellApplication {
  name = "swww-randomize";
  runtimeInputs = [
    coreutils
    swww
  ];
  text = builtins.readFile ./script.sh;
}
