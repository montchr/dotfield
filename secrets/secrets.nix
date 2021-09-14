{ config }:
{
  "testSecret.age".publicKeys = [ config.my.keys.ssh ];
}
