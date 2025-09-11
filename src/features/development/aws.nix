{
  aspects.development.home =
    { pkgs, ... }:
    {
      home.packages = [
        pkgs.awscli2

        # https://towardsthecloud.com/set-up-aws-cli-aws-sso
        # https://www.granted.dev/
        # https://github.com/common-fate/granted
        # the aws cli is so terrible that it needs a helper just to log in with sso
        # (and aws really pushes sso hard, oops)
        # and unfortunately, the static access keys reset every 10 minutes or so,
        # making even the simple escape hatch unbearable. so much time wasted
        # waiting for an rclone s3 command to complete only to see that the tokens expired.
        pkgs.granted
      ];
    };
}
