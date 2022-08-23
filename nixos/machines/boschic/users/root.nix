{config, ...}: {
  sops.secrets."users/root/passphrase".neededForUsers = true;
  users.users.root.initialHashedPassword = "$6$HshRirQmQu.nxnwE$6eUWz9pN3T9F4KZVBpz7KfvZhLAFRGRHkm1YFsIqpQUSHBw8Lfh6G6PBLbHp9/XUxiIz0MZQaxRqQvHMIn/hW0";
  users.users.root.passwordFile = config.sops.secrets."users/root/passphrase".path;
}
