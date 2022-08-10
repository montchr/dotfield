final: prev: {
  hpi = final.callPackage ./HPI {};
  orgparse = final.callPackage ./orgparse {};
  promnesia = final.callPackage ./promnesia {
    inherit (final) hpi orgparse;
  };
}
