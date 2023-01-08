{
  l,
  supportedSystems,
  ...
}: rec {
  isDarwin = l.hasSuffix "-darwin";
  isLinux = l.hasSuffix "-linux";
  eachDarwinSystem = l.genAttrs (l.filter isDarwin supportedSystems);
  eachLinuxSystem = l.genAttrs (l.filter isLinux supportedSystems);
}
