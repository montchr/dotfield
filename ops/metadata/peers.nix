let
  hosts = builtins.fromTOML (builtins.readFile ./hosts.toml);
  networks = builtins.fromTOML (builtins.readFile ./networks.toml);
in {inherit hosts networks;}
