rec {
  firstName = "Chris";
  lastName = "Montgomery";
  name = "${firstName} ${lastName}";

  timezone = "America/New_York";

  website = "https://github.com/montchr";
  username = "montchr";
  usernames = {
    github = "montchr";
    gitlab = "montchr";
    sourcehut = "montchr";
  };

  email = emails.personal;
  emails = {
    personal = "chris@cdom.io";
    work = "chris@alley.co";
  };

  keys = {
    pgp = "0x135EEDD0F71934F3";
    ssh = {
      primary = import ./ssh-primary-key.nix;
      identities = import ./ssh-identities.nix;
      hostKeyPaths = [
        "~/.ssh/id_ed25519"
        # TODO: filter non-existant paths
        # "~/.ssh/id_rsa"
      ];
    };
  };
}
