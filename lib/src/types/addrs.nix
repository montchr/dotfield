{
  l,
  lib,
  ...
}: let
  inherit (lib.strings) test;
  t = l.types;
  addr = t.either addr4 addr6;
  addr4 = l.mkOptionType {
    name = "IPv4 address";
    check = let
      IPv4address = let
        d = "([1-9]?[0-9]|1[0-9][0-9]|2[0-4][0-9]|25[0-5])";
      in
        l.concatMapStringsSep "." (l.const d) (l.range 1 4);
    in
      test IPv4address;
    merge = l.mergeOneOption;
  };
  addr6 = l.mkOptionType {
    name = "IPv6 address";
    check = let
      # TODO(stockholm): check IPv6 address harder
      IPv6address = "[0-9a-f.:]+";
    in
      test IPv6address;
    merge = l.mergeOneOption;
  };

  cidr = t.either cidr4 cidr6;
  cidr4 = l.mkOptionType {
    name = "CIDRv4 address";
    check = let
      CIDRv4address = let
        d = "([1-9]?[0-9]|1[0-9][0-9]|2[0-4][0-9]|25[0-5])";
      in
        l.concatMapStringsSep "." (l.const d) (l.range 1 4) + "(/([1-2]?[0-9]|3[0-2]))?";
    in
      test CIDRv4address;
    merge = l.mergeOneOption;
  };
  cidr6 = l.mkOptionType {
    name = "CIDRv6 address";
    check = let
      # TODO(stockholm): check IPv6 address harder
      CIDRv6address = "[0-9a-f.:]+(/([0-9][0-9]?|1[0-2][0-8]))?";
    in
      test CIDRv6address;
    merge = l.mergeOneOption;
  };
in {inherit addr addr4 addr6 cidr cidr4 cidr6;}
