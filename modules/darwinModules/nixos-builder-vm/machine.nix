{
  hostName,
  maxJobs,
  system,
}: rec {
  inherit hostName maxJobs system;
  publicHostKey = "c3NoLWVkMjU1MTkgQUFBQUMzTnphQzFsWkRJMU5URTVBQUFBSUpCV2N4Yi9CbGFxdDFhdU90RStGOFFVV3JVb3RpQzVxQkorVXVFV2RWQ2Igcm9vdEBuaXhvcwo=";
  sshKey = "/etc/nix/${sshUser}_ed25519";
  sshUser = "builder";
}
