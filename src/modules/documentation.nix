{
  dotfield.modules.workstation.nixos = {
    # NOTE: This will significantly slow down builds.  However, it enables more
    # manpage integrations across various tools (e.g. `apropos`, `man -k`).
    documentation.man.generateCaches = true;
  };
}
