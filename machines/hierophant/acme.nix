{ops, ...}: {
  security.acme.acceptTerms = true;
  security.acme.defaults.email = ops.networks.seadome.contact;
}
