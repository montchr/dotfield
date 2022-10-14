{
  buildGoModule,
  go,
  lib,
  source,
}:
buildGoModule rec {
  inherit (source) pname version src;
  vendorSha256 = "sha256-iq4hKZHDJ0aFXweBHCATqO5ZOp2VuaLDpzbZpxDWuOg=";
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
