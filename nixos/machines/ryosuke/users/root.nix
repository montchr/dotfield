{
  config,
  lib,
  pkgs,
  ...
}: {
  sops.secrets."users/root/passphrase".neededForUsers = true;
  users.users.root.initialHashedPassword = "$6$0H8NbKLF5uDD.rvG$S46H2N8W0wKiRRCZOlE5QXBxZCd9CEU0rRi5kZdLMfvcMaYGMC9OEojjAW9i/3c6vRktxQnSUwv4xIZlOOjlB/";
  users.users.root.passwordFile = config.sops.secrets."users/root/passphrase".path;
}
