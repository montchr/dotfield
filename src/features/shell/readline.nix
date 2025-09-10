{
  aspects.core.home = {
    programs.readline.enable = true;
    programs.readline.variables = {
      # Expand tilde to home directory.
      expand-tilde = true;
    };
  };
}
