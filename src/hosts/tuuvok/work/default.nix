{
  dotfield.hosts.nixos.tuuvok.nixos = {
    security.pki.certificates = [
      # mkcert for local development
      (builtins.readFile ./rootCA.crt)
    ];
  };
}
