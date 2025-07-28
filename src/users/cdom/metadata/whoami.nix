{
  dotfield.meta.users.cdom.whoami = {
    name = "Chris Montgomery";
    firstName = "Chris";
    lastName = "Montgomery";
    email = "chmont@protonmail.com";
    pgp = rec {
      id = "0x135EEDD0F71934F3";
      # TODO: how can this be made a default?  perhaps there is indeed
      # a benefit to centralising keys.
      key = builtins.readFile ./keys/${id}.asc;
    };
    github = "montchr";
    mastodon = "@montchr@assemblag.es";
  };
}
