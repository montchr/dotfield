let
  alleymon = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIGk9fhwXG95cVD9DLsHuXrdJYs8DsUF/AmYWcO1+bPVd alleymon";
  hostKeys = [ alleymon ];
in

{
  "testSecret.age".publicKeys = hostKeys;
}
