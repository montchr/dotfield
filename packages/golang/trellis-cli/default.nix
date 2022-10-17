{
  buildGoModule,
  go,
  lib,
  source,
}:
buildGoModule rec {
  inherit (source) pname version src;
  vendorSha256 = "sha256-xsOJNFDgZJ0M69Q/r8fPcv4SfACLgknADb8gEZ4+n50=";
  # FIXME: tests run, but fail -- may need some deps available, like ssh-keygen?
  doCheck = false;
  TEST_BINARY = "${go}/bin/go";
  meta = with lib; {
    description = "A CLI to manage Trellis projects";
    homepage = "https://github.com/roots/trellis-cli";
    license = licenses.mit;
    maintainers = with maintainers; [montchr];
  };
}
