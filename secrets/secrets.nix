{ config }:
{
  "testSecret.txt".publicKeys = config.my.keys.ssh;
}
