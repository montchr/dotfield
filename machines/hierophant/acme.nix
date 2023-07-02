{ops, ...}: {
  security.acme.acceptTerms = true;
  security.acme.defaults.email = ops.metadata.networks.seadome.contact;
}
