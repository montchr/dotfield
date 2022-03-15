{
  composerEnv,
  fetchurl,
  fetchgit ? null,
  fetchhg ? null,
  fetchsvn ? null,
  noDev ? false,
}: let
  packages = {
    "amphp/amp" = {
      targetDir = "";
      src = composerEnv.buildZipPackage {
        name = "amphp-amp-8c5a6bce16a7bcd0f7f3104f0af1ee03b463f20b";
        src = fetchurl {
          url = "https://api.github.com/repos/amphp/amp/zipball/8c5a6bce16a7bcd0f7f3104f0af1ee03b463f20b";
          sha256 = "028qxbi3fbc1ci9i8z0qsg9lc9zqfkid8ipd285zfzzdw7n2yd7v";
        };
      };
    };
    "amphp/byte-stream" = {
      targetDir = "";
      src = composerEnv.buildZipPackage {
        name = "amphp-byte-stream-7a64a9ad336fc5e1e70b1c1fc1e9618a7027332e";
        src = fetchurl {
          url = "https://api.github.com/repos/amphp/byte-stream/zipball/7a64a9ad336fc5e1e70b1c1fc1e9618a7027332e";
          sha256 = "032pq6xz4z2pzy9vs3cfl8036xi2nxlvjvflk8wqflhyqfmi1mww";
        };
      };
    };
    "amphp/cache" = {
      targetDir = "";
      src = composerEnv.buildZipPackage {
        name = "amphp-cache-2b6b5dbb70e54cc914df9952ba7c012bc4cbcd28";
        src = fetchurl {
          url = "https://api.github.com/repos/amphp/cache/zipball/2b6b5dbb70e54cc914df9952ba7c012bc4cbcd28";
          sha256 = "0ph57sarmqihnnqlsffjf6ajihgk2sq9zq4vyrfbz3jshqikwm71";
        };
      };
    };
    "amphp/dns" = {
      targetDir = "";
      src = composerEnv.buildZipPackage {
        name = "amphp-dns-852292532294d7972c729a96b49756d781f7c59d";
        src = fetchurl {
          url = "https://api.github.com/repos/amphp/dns/zipball/852292532294d7972c729a96b49756d781f7c59d";
          sha256 = "1l2k427x51an2y7531vcw0gbs3gxvm5ni8b82ahnxq71h36js5bk";
        };
      };
    };
    "amphp/parser" = {
      targetDir = "";
      src = composerEnv.buildZipPackage {
        name = "amphp-parser-f83e68f03d5b8e8e0365b8792985a7f341c57ae1";
        src = fetchurl {
          url = "https://api.github.com/repos/amphp/parser/zipball/f83e68f03d5b8e8e0365b8792985a7f341c57ae1";
          sha256 = "1qda6falmlgwvwcrbczzxalq6mhvmls5grzpzr5saf84107dn6j7";
        };
      };
    };
    "amphp/process" = {
      targetDir = "";
      src = composerEnv.buildZipPackage {
        name = "amphp-process-b88c6aef75c0b22f6f021141dd2d5e7c5db4c124";
        src = fetchurl {
          url = "https://api.github.com/repos/amphp/process/zipball/b88c6aef75c0b22f6f021141dd2d5e7c5db4c124";
          sha256 = "0c7ycya3320z2vxw99px8wqis0i5qypllvgrlgmqbwc09225y3kg";
        };
      };
    };
    "amphp/serialization" = {
      targetDir = "";
      src = composerEnv.buildZipPackage {
        name = "amphp-serialization-693e77b2fb0b266c3c7d622317f881de44ae94a1";
        src = fetchurl {
          url = "https://api.github.com/repos/amphp/serialization/zipball/693e77b2fb0b266c3c7d622317f881de44ae94a1";
          sha256 = "14mx5540f1z672fkszdc5qcdz370i3q7w0kdl87aimzj87r3awkx";
        };
      };
    };
    "amphp/socket" = {
      targetDir = "";
      src = composerEnv.buildZipPackage {
        name = "amphp-socket-a8af9f5d0a66c5fe9567da45a51509e592788fe6";
        src = fetchurl {
          url = "https://api.github.com/repos/amphp/socket/zipball/a8af9f5d0a66c5fe9567da45a51509e592788fe6";
          sha256 = "0aapwq1jz2dvc638cpfp12n4fgwmlcrlrqbkrm6prxdbzh2yaiwv";
        };
      };
    };
    "amphp/sync" = {
      targetDir = "";
      src = composerEnv.buildZipPackage {
        name = "amphp-sync-613047ac54c025aa800a9cde5b05c3add7327ed4";
        src = fetchurl {
          url = "https://api.github.com/repos/amphp/sync/zipball/613047ac54c025aa800a9cde5b05c3add7327ed4";
          sha256 = "13v6355rghmwjq49y252j5q91c6igb3vbv9sjfhdhl2mgs5r6dmy";
        };
      };
    };
    "amphp/windows-registry" = {
      targetDir = "";
      src = composerEnv.buildZipPackage {
        name = "amphp-windows-registry-0f56438b9197e224325e88f305346f0221df1f71";
        src = fetchurl {
          url = "https://api.github.com/repos/amphp/windows-registry/zipball/0f56438b9197e224325e88f305346f0221df1f71";
          sha256 = "1vv8xik6swpy12c5nzgfwrnjm92ay7v8vlwjw3wq0vjlrrkjw0jq";
        };
      };
    };
    "brick/math" = {
      targetDir = "";
      src = composerEnv.buildZipPackage {
        name = "brick-math-ca57d18f028f84f777b2168cd1911b0dee2343ae";
        src = fetchurl {
          url = "https://api.github.com/repos/brick/math/zipball/ca57d18f028f84f777b2168cd1911b0dee2343ae";
          sha256 = "1nr1grrb9g5g3ihx94yk0amp8zx8prkkvg2934ygfc3rrv03cq9w";
        };
      };
    };
    "composer/ca-bundle" = {
      targetDir = "";
      src = composerEnv.buildZipPackage {
        name = "composer-ca-bundle-98fc23fde70b2cb3134ae56614064ab8780283e1";
        src = fetchurl {
          url = "https://api.github.com/repos/composer/ca-bundle/zipball/98fc23fde70b2cb3134ae56614064ab8780283e1";
          sha256 = "1ya7as990zq2mxr52hrz7szbzrw8jbln3amwai8lrhq7qrii36m9";
        };
      };
    };
    "composer/composer" = {
      targetDir = "";
      src = composerEnv.buildZipPackage {
        name = "composer-composer-32eb3b459a17d263a52c79e459d7d76e5e1bfb12";
        src = fetchurl {
          url = "https://api.github.com/repos/composer/composer/zipball/32eb3b459a17d263a52c79e459d7d76e5e1bfb12";
          sha256 = "1q913f18bqwh0zbly9lh8wd3hdbpd8z16p60fbxgbn6c8i183mb7";
        };
      };
    };
    "composer/package-versions-deprecated" = {
      targetDir = "";
      src = composerEnv.buildZipPackage {
        name = "composer-package-versions-deprecated-fff576ac850c045158a250e7e27666e146e78d18";
        src = fetchurl {
          url = "https://api.github.com/repos/composer/package-versions-deprecated/zipball/fff576ac850c045158a250e7e27666e146e78d18";
          sha256 = "1r78nzvz6w1kh3vp9sdqg48cc3kbb7ign6s6dhkx9dpxgp2bv2ir";
        };
      };
    };
    "composer/semver" = {
      targetDir = "";
      src = composerEnv.buildZipPackage {
        name = "composer-semver-647490bbcaf7fc4891c58f47b825eb99d19c377a";
        src = fetchurl {
          url = "https://api.github.com/repos/composer/semver/zipball/647490bbcaf7fc4891c58f47b825eb99d19c377a";
          sha256 = "16dx37b0b3qnla05h8l49hyg6khibd52i42lwqfyd91iysdpgz5r";
        };
      };
    };
    "composer/spdx-licenses" = {
      targetDir = "";
      src = composerEnv.buildZipPackage {
        name = "composer-spdx-licenses-056625390d33e3ff2c887488ac642e0fa6dce205";
        src = fetchurl {
          url = "https://api.github.com/repos/composer/spdx-licenses/zipball/056625390d33e3ff2c887488ac642e0fa6dce205";
          sha256 = "112cad22w2l3rcwwxknjss4q23dkr6504mqsarcrjzhd1pq4r53i";
        };
      };
    };
    "composer/xdebug-handler" = {
      targetDir = "";
      src = composerEnv.buildZipPackage {
        name = "composer-xdebug-handler-f27e06cd9675801df441b3656569b328e04aa37c";
        src = fetchurl {
          url = "https://api.github.com/repos/composer/xdebug-handler/zipball/f27e06cd9675801df441b3656569b328e04aa37c";
          sha256 = "0db49yf7zcf4q57ba48n10cyrdjf7s598321m69dkb4dph0yc5qh";
        };
      };
    };
    "dantleech/argument-resolver" = {
      targetDir = "";
      src = composerEnv.buildZipPackage {
        name = "dantleech-argument-resolver-e34fabf7d6e53e5194f745ad069c4a87cc4b34cc";
        src = fetchurl {
          url = "https://gitlab.com/api/v4/projects/dantleech%2Fargument-resolver/repository/archive.zip?sha=e34fabf7d6e53e5194f745ad069c4a87cc4b34cc";
          sha256 = "023hap8ikywq34j95xpb405hpi1fj9yp5za9a8ky9il87kdhsvnb";
        };
      };
    };
    "dantleech/invoke" = {
      targetDir = "";
      src = composerEnv.buildZipPackage {
        name = "dantleech-invoke-9b002d746d2c1b86cfa63a47bb5909cee58ef50c";
        src = fetchurl {
          url = "https://api.github.com/repos/dantleech/invoke/zipball/9b002d746d2c1b86cfa63a47bb5909cee58ef50c";
          sha256 = "165vlqj5rf33gwvgc7674qxc12kqbpi7dqbzcdr87d4v6vi99w9n";
        };
      };
    };
    "dantleech/object-renderer" = {
      targetDir = "";
      src = composerEnv.buildZipPackage {
        name = "dantleech-object-renderer-942ad54a22e5ffb9ac3421d7bb06fa76bc45ad30";
        src = fetchurl {
          url = "https://api.github.com/repos/dantleech/object-renderer/zipball/942ad54a22e5ffb9ac3421d7bb06fa76bc45ad30";
          sha256 = "1m3dgyq1bs8xgffawdl3yij9zq9bcv6xay18p9zi9zq9wxawpzwj";
        };
      };
    };
    "daverandom/libdns" = {
      targetDir = "";
      src = composerEnv.buildZipPackage {
        name = "daverandom-libdns-e8b6d6593d18ac3a6a14666d8a68a4703b2e05f9";
        src = fetchurl {
          url = "https://api.github.com/repos/DaveRandom/LibDNS/zipball/e8b6d6593d18ac3a6a14666d8a68a4703b2e05f9";
          sha256 = "0l84mrkmm5w2cpkxvacm31vmv7pbz4dyxs5fj1rjfvbrhs0c2x03";
        };
      };
    };
    "dnoegel/php-xdg-base-dir" = {
      targetDir = "";
      src = composerEnv.buildZipPackage {
        name = "dnoegel-php-xdg-base-dir-8f8a6e48c5ecb0f991c2fdcf5f154a47d85f9ffd";
        src = fetchurl {
          url = "https://api.github.com/repos/dnoegel/php-xdg-base-dir/zipball/8f8a6e48c5ecb0f991c2fdcf5f154a47d85f9ffd";
          sha256 = "02n4b4wkwncbqiz8mw2rq35flkkhn7h6c0bfhjhs32iay1y710fq";
        };
      };
    };
    "jetbrains/phpstorm-stubs" = {
      targetDir = "";
      src = composerEnv.buildZipPackage {
        name = "jetbrains-phpstorm-stubs-82595d7a426c4b3d1e3a7d604ad3f99534784599";
        src = fetchurl {
          url = "https://api.github.com/repos/JetBrains/phpstorm-stubs/zipball/82595d7a426c4b3d1e3a7d604ad3f99534784599";
          sha256 = "0bjm8svma4sb8rxf1977adrgm0bs8wagv4hr3n7nh550wj8c1f8j";
        };
      };
    };
    "justinrainbow/json-schema" = {
      targetDir = "";
      src = composerEnv.buildZipPackage {
        name = "justinrainbow-json-schema-2ab6744b7296ded80f8cc4f9509abbff393399aa";
        src = fetchurl {
          url = "https://api.github.com/repos/justinrainbow/json-schema/zipball/2ab6744b7296ded80f8cc4f9509abbff393399aa";
          sha256 = "0mfi1w83hcynqhjbgz4ljpyyiigyvi984q3ya02xkzvrxpp3hiwg";
        };
      };
    };
    "kelunik/certificate" = {
      targetDir = "";
      src = composerEnv.buildZipPackage {
        name = "kelunik-certificate-47dcc659fe73ae040ae17d068a4fc8777813f0fb";
        src = fetchurl {
          url = "https://api.github.com/repos/kelunik/certificate/zipball/47dcc659fe73ae040ae17d068a4fc8777813f0fb";
          sha256 = "17llzncir8kj7yjqwnly204598rxlm4fpc2qygzds1wxp9gvg119";
        };
      };
    };
    "league/uri-parser" = {
      targetDir = "";
      src = composerEnv.buildZipPackage {
        name = "league-uri-parser-e6eb42f951433aa3106639b6592f1ef9f8a36d40";
        src = fetchurl {
          url = "https://api.github.com/repos/thephpleague/uri-parser/zipball/e6eb42f951433aa3106639b6592f1ef9f8a36d40";
          sha256 = "1pw0xjygnwzqs08dl3999cm81zx6ingwa39pwr15yhqpnydbs1i0";
        };
      };
    };
    "microsoft/tolerant-php-parser" = {
      targetDir = "";
      src = composerEnv.buildZipPackage {
        name = "microsoft-tolerant-php-parser-1d76657e3271754515ace52501d3e427eca42ad0";
        src = fetchurl {
          url = "https://api.github.com/repos/microsoft/tolerant-php-parser/zipball/1d76657e3271754515ace52501d3e427eca42ad0";
          sha256 = "0dhyv0y0l161ccxq9l6xaqik1w7n9a5izpy5b4rq1s0pn2k05mik";
        };
      };
    };
    "monolog/monolog" = {
      targetDir = "";
      src = composerEnv.buildZipPackage {
        name = "monolog-monolog-c6b00f05152ae2c9b04a448f99c7590beb6042f5";
        src = fetchurl {
          url = "https://api.github.com/repos/Seldaek/monolog/zipball/c6b00f05152ae2c9b04a448f99c7590beb6042f5";
          sha256 = "02hr0z3rshvk7hiva7ag3rblr1wymm6s7s9i2yy5bai8f2qwjvdf";
        };
      };
    };
    "phpactor/amp-fswatch" = {
      targetDir = "";
      src = composerEnv.buildZipPackage {
        name = "phpactor-amp-fswatch-e40b7dc1b96c5fdb5c6598a9abe9ca846039cdf1";
        src = fetchurl {
          url = "https://api.github.com/repos/phpactor/amp-fswatch/zipball/e40b7dc1b96c5fdb5c6598a9abe9ca846039cdf1";
          sha256 = "0x31612vgc2528jrcj54zn125ad5sl3m4m2rnamv4b8mp0drf577";
        };
      };
    };
    "phpactor/class-mover" = {
      targetDir = "";
      src = composerEnv.buildZipPackage {
        name = "phpactor-class-mover-47166e1fb095ff178dacb7a1b765b5c6e59f42e5";
        src = fetchurl {
          url = "https://api.github.com/repos/phpactor/class-mover/zipball/47166e1fb095ff178dacb7a1b765b5c6e59f42e5";
          sha256 = "1mjg0jcls3kc7i4wawsq902nz51llp70i0mlw7rcwkqg53xaj1sq";
        };
      };
    };
    "phpactor/class-to-file" = {
      targetDir = "";
      src = composerEnv.buildZipPackage {
        name = "phpactor-class-to-file-fa9ed96dec151e59fbd9d0583f5d08b9f02070f7";
        src = fetchurl {
          url = "https://api.github.com/repos/phpactor/class-to-file/zipball/fa9ed96dec151e59fbd9d0583f5d08b9f02070f7";
          sha256 = "1x2pbz555bd5z94r6ncqyzlhl331s7qgccfrb5wnrslhny8fr5f6";
        };
      };
    };
    "phpactor/class-to-file-extension" = {
      targetDir = "";
      src = composerEnv.buildZipPackage {
        name = "phpactor-class-to-file-extension-9d6f6ae97f2642d79c1a8ab863d7512b347d856b";
        src = fetchurl {
          url = "https://api.github.com/repos/phpactor/class-to-file-extension/zipball/9d6f6ae97f2642d79c1a8ab863d7512b347d856b";
          sha256 = "06nigacwlx893812fyyf3kjg9q5mpcyk5rjgjn9xdn2f5y1l1zzp";
        };
      };
    };
    "phpactor/code-builder" = {
      targetDir = "";
      src = composerEnv.buildZipPackage {
        name = "phpactor-code-builder-9d739939a19e497b8da4e2704a3d102b4600aaf9";
        src = fetchurl {
          url = "https://api.github.com/repos/phpactor/code-builder/zipball/9d739939a19e497b8da4e2704a3d102b4600aaf9";
          sha256 = "07h9nfa5ykicvlxhdvsijhz46g6s8acl34niqz7lcdd1qhz3dffb";
        };
      };
    };
    "phpactor/code-transform" = {
      targetDir = "";
      src = composerEnv.buildZipPackage {
        name = "phpactor-code-transform-ec78cbb60d4451204a13962570c9cb5e3b687d3d";
        src = fetchurl {
          url = "https://api.github.com/repos/phpactor/code-transform/zipball/ec78cbb60d4451204a13962570c9cb5e3b687d3d";
          sha256 = "09mhsf1yqkziwk76q2szij57h9zm04j1wyzsp6sch2ay9vp721zi";
        };
      };
    };
    "phpactor/code-transform-extension" = {
      targetDir = "";
      src = composerEnv.buildZipPackage {
        name = "phpactor-code-transform-extension-aff7ebc4127882adbf1ead01a86adb5ce167baff";
        src = fetchurl {
          url = "https://api.github.com/repos/phpactor/code-transform-extension/zipball/aff7ebc4127882adbf1ead01a86adb5ce167baff";
          sha256 = "063jx6rm26ampa7ls0blzmaazb1zbi8fqvw54gd9jg9cgrxqnd3v";
        };
      };
    };
    "phpactor/completion" = {
      targetDir = "";
      src = composerEnv.buildZipPackage {
        name = "phpactor-completion-e4486dc846b26eb3d1019fceab21c43fc0609e2f";
        src = fetchurl {
          url = "https://api.github.com/repos/phpactor/completion/zipball/e4486dc846b26eb3d1019fceab21c43fc0609e2f";
          sha256 = "06s91zmdfmg2s1378x8ispjzp5cm4mkgzhgl8ww53kiclxx2n30p";
        };
      };
    };
    "phpactor/completion-extension" = {
      targetDir = "";
      src = composerEnv.buildZipPackage {
        name = "phpactor-completion-extension-1867de2160db1291f94e18697ec1491bdf450a33";
        src = fetchurl {
          url = "https://api.github.com/repos/phpactor/completion-extension/zipball/1867de2160db1291f94e18697ec1491bdf450a33";
          sha256 = "0jz4fgrk3zvw5n7c7sz1jpicy8bm3xf49spz1pmgpkq3av5mgwrc";
        };
      };
    };
    "phpactor/completion-rpc-extension" = {
      targetDir = "";
      src = composerEnv.buildZipPackage {
        name = "phpactor-completion-rpc-extension-5f57f968949554559b75f6bc652a23159959916c";
        src = fetchurl {
          url = "https://api.github.com/repos/phpactor/completion-rpc-extension/zipball/5f57f968949554559b75f6bc652a23159959916c";
          sha256 = "1qdk4ffcr5xwz8f383fjs6idwwg73914irvllgbjdjpv7g16mkl7";
        };
      };
    };
    "phpactor/completion-worse-extension" = {
      targetDir = "";
      src = composerEnv.buildZipPackage {
        name = "phpactor-completion-worse-extension-e1aceb0834b2bcd4543a09798d2cff124e648424";
        src = fetchurl {
          url = "https://api.github.com/repos/phpactor/completion-worse-extension/zipball/e1aceb0834b2bcd4543a09798d2cff124e648424";
          sha256 = "0n7igvznc9kmnipg0ilrrvp5m0pl1lwrwysprq2wv7j0irc0n88g";
        };
      };
    };
    "phpactor/composer-autoloader-extension" = {
      targetDir = "";
      src = composerEnv.buildZipPackage {
        name = "phpactor-composer-autoloader-extension-442bfb7667d4c75e64487d6c4228d24eed0f6e66";
        src = fetchurl {
          url = "https://api.github.com/repos/phpactor/composer-autoloader-extension/zipball/442bfb7667d4c75e64487d6c4228d24eed0f6e66";
          sha256 = "1z5w45w4bgk4asvq7qdmn0fa4pxfdph8wq5l54jhbb1rk3f8j1hd";
        };
      };
    };
    "phpactor/config-loader" = {
      targetDir = "";
      src = composerEnv.buildZipPackage {
        name = "phpactor-config-loader-297608563ccdc14874d090c5a9af7d6c579b1a36";
        src = fetchurl {
          url = "https://api.github.com/repos/phpactor/config-loader/zipball/297608563ccdc14874d090c5a9af7d6c579b1a36";
          sha256 = "1dszx28jwd0a228xm0gkg5f9gb6grd9izsnrdl8xynqfwjjymmwr";
        };
      };
    };
    "phpactor/console-extension" = {
      targetDir = "";
      src = composerEnv.buildZipPackage {
        name = "phpactor-console-extension-f4d2161287e71751539fb5647ab5d20c0f2b8675";
        src = fetchurl {
          url = "https://api.github.com/repos/phpactor/console-extension/zipball/f4d2161287e71751539fb5647ab5d20c0f2b8675";
          sha256 = "14pp8blz0w6a23vmb5xb5k63kw8b3r5ck6349dgz6glq7q4b58y2";
        };
      };
    };
    "phpactor/container" = {
      targetDir = "";
      src = composerEnv.buildZipPackage {
        name = "phpactor-container-a517b095e07c6af51c402aa8f4c39f0cfd59deb1";
        src = fetchurl {
          url = "https://api.github.com/repos/phpactor/container/zipball/a517b095e07c6af51c402aa8f4c39f0cfd59deb1";
          sha256 = "0vw7bc1fi4mgidqss405a5kfmc7grhaf9ijyx1rsxnhsw0sa3nbk";
        };
      };
    };
    "phpactor/docblock" = {
      targetDir = "";
      src = composerEnv.buildZipPackage {
        name = "phpactor-docblock-73db4e18b4faea9fdd2c6211dacff04f432992a9";
        src = fetchurl {
          url = "https://api.github.com/repos/phpactor/docblock/zipball/73db4e18b4faea9fdd2c6211dacff04f432992a9";
          sha256 = "0na8w21wh2qysra8jgz6y0pac14rgs6x8lbwdv01b7xcqwanphic";
        };
      };
    };
    "phpactor/extension-manager-extension" = {
      targetDir = "";
      src = composerEnv.buildZipPackage {
        name = "phpactor-extension-manager-extension-399a722367ce771e2f6f070a378e53eac83efe99";
        src = fetchurl {
          url = "https://api.github.com/repos/phpactor/extension-manager-extension/zipball/399a722367ce771e2f6f070a378e53eac83efe99";
          sha256 = "0ycrxhsl7h3zmy155m5mnijzizq96sdag7133ny3v3wxcax6bpxg";
        };
      };
    };
    "phpactor/file-path-resolver" = {
      targetDir = "";
      src = composerEnv.buildZipPackage {
        name = "phpactor-file-path-resolver-d274de198741a7b91bd657fe68173b6d8c5aa3f3";
        src = fetchurl {
          url = "https://api.github.com/repos/phpactor/file-path-resolver/zipball/d274de198741a7b91bd657fe68173b6d8c5aa3f3";
          sha256 = "1fip85nlyx2lza2g6vx3lz9f6cmxqnw24a4cmxf6gxar8l3xi1j7";
        };
      };
    };
    "phpactor/file-path-resolver-extension" = {
      targetDir = "";
      src = composerEnv.buildZipPackage {
        name = "phpactor-file-path-resolver-extension-730b46357f4f61209e2ae17bd45cfaf0a7a4c75b";
        src = fetchurl {
          url = "https://api.github.com/repos/phpactor/file-path-resolver-extension/zipball/730b46357f4f61209e2ae17bd45cfaf0a7a4c75b";
          sha256 = "167f5r46xs61lyabcp3s68gmdadswksyik0ikjkp0hkpx65qa2ql";
        };
      };
    };
    "phpactor/indexer-extension" = {
      targetDir = "";
      src = composerEnv.buildZipPackage {
        name = "phpactor-indexer-extension-537febc6c3995645a1feaaf6b7c51d637ec9a501";
        src = fetchurl {
          url = "https://api.github.com/repos/phpactor/indexer-extension/zipball/537febc6c3995645a1feaaf6b7c51d637ec9a501";
          sha256 = "1mgvlbbn1xbl148l1c6ic13v2v3j23bc5yypnq11mj6isg1n9fnd";
        };
      };
    };
    "phpactor/language-server" = {
      targetDir = "";
      src = composerEnv.buildZipPackage {
        name = "phpactor-language-server-ac37100b331c9b79b4cbb9095c13c71976325782";
        src = fetchurl {
          url = "https://api.github.com/repos/phpactor/language-server/zipball/ac37100b331c9b79b4cbb9095c13c71976325782";
          sha256 = "0y0gmkw5j39jvcm6f549nnbda18hlalrpg6g93da5wsgdfxnj315";
        };
      };
    };
    "phpactor/language-server-extension" = {
      targetDir = "";
      src = composerEnv.buildZipPackage {
        name = "phpactor-language-server-extension-0e603fd6ef43816686b5261cbf7364d1fdbb219a";
        src = fetchurl {
          url = "https://api.github.com/repos/phpactor/language-server-extension/zipball/0e603fd6ef43816686b5261cbf7364d1fdbb219a";
          sha256 = "1h9fhcsik24jz6ndn73g9qkiqdv4slmkbw16f2bvgq8zlmak7qy6";
        };
      };
    };
    "phpactor/language-server-phpactor-extensions" = {
      targetDir = "";
      src = composerEnv.buildZipPackage {
        name = "phpactor-language-server-phpactor-extensions-66b93056a69d3de051b80634c6530038c0b684fc";
        src = fetchurl {
          url = "https://api.github.com/repos/phpactor/language-server-phpactor-extensions/zipball/66b93056a69d3de051b80634c6530038c0b684fc";
          sha256 = "1l44zzn8x4i11dh595q5xil5p7ln8q4hd1q6xa4ld44w4pyi88sk";
        };
      };
    };
    "phpactor/language-server-protocol" = {
      targetDir = "";
      src = composerEnv.buildZipPackage {
        name = "phpactor-language-server-protocol-393e8f22392f77e6581d3578d82a0b9ed4f73a26";
        src = fetchurl {
          url = "https://api.github.com/repos/phpactor/language-server-protocol/zipball/393e8f22392f77e6581d3578d82a0b9ed4f73a26";
          sha256 = "16sspmgwlwdgpxjyizk3pd1khrgx8fjspv9xscgjvq1wl0ffh6k4";
        };
      };
    };
    "phpactor/logging-extension" = {
      targetDir = "";
      src = composerEnv.buildZipPackage {
        name = "phpactor-logging-extension-1799922042200683782f563d5cec3fbf1a85c8c7";
        src = fetchurl {
          url = "https://api.github.com/repos/phpactor/logging-extension/zipball/1799922042200683782f563d5cec3fbf1a85c8c7";
          sha256 = "0ijyk481iqyzcqjd6nq25s7hcylnxwwn9b789pz9mdgzs9swpb2b";
        };
      };
    };
    "phpactor/map-resolver" = {
      targetDir = "";
      src = composerEnv.buildZipPackage {
        name = "phpactor-map-resolver-0a1b591fc7d059717fa93bfef97417e4c375be74";
        src = fetchurl {
          url = "https://api.github.com/repos/phpactor/map-resolver/zipball/0a1b591fc7d059717fa93bfef97417e4c375be74";
          sha256 = "05hashycf6kvbvfwpjayf200rqpqcw4zaq5gan2vydsr1cv8rl23";
        };
      };
    };
    "phpactor/name-specification" = {
      targetDir = "";
      src = composerEnv.buildZipPackage {
        name = "phpactor-name-specification-ebb0a9a0589a7992b97075441bfc7def4e01889d";
        src = fetchurl {
          url = "https://api.github.com/repos/phpactor/name/zipball/ebb0a9a0589a7992b97075441bfc7def4e01889d";
          sha256 = "0s4bzxz94jfyipwwi66i3lzdgnc3djmw0yss3shrczp3q19iyczp";
        };
      };
    };
    "phpactor/path-finder" = {
      targetDir = "";
      src = composerEnv.buildZipPackage {
        name = "phpactor-path-finder-ff3c8e1cc61b807cc4b04d45b473026df16bcd16";
        src = fetchurl {
          url = "https://api.github.com/repos/phpactor/path-finder/zipball/ff3c8e1cc61b807cc4b04d45b473026df16bcd16";
          sha256 = "1a5m7azgjfcykhhqvsm1pfmxc0pa9j3gm0i0dna6j8a1gg4f9a80";
        };
      };
    };
    "phpactor/phly-event-dispatcher" = {
      targetDir = "";
      src = composerEnv.buildZipPackage {
        name = "phpactor-phly-event-dispatcher-5519ac1a5df8a1db72df82e11367b23443f2a0fe";
        src = fetchurl {
          url = "https://api.github.com/repos/phpactor/phly-event-dispatcher/zipball/5519ac1a5df8a1db72df82e11367b23443f2a0fe";
          sha256 = "1y8j5c1plmwbfyjja95fl721jf8r7lbivqa6m8hmi2s9hql3bx4n";
        };
      };
    };
    "phpactor/php-extension" = {
      targetDir = "";
      src = composerEnv.buildZipPackage {
        name = "phpactor-php-extension-3a9739072bf9723c31b11da0dbf17aef7bc639e0";
        src = fetchurl {
          url = "https://api.github.com/repos/phpactor/php-extension/zipball/3a9739072bf9723c31b11da0dbf17aef7bc639e0";
          sha256 = "0wc4p6v1dzsxc8layk0knfbz4f506di03jal9qld7aijhigh4i0p";
        };
      };
    };
    "phpactor/phpactor" = {
      targetDir = "";
      src = composerEnv.buildZipPackage {
        name = "phpactor-phpactor-1194ce124a358e115d8e88c9309367cd7acf62ba";
        src = fetchurl {
          url = "https://api.github.com/repos/phpactor/phpactor/zipball/1194ce124a358e115d8e88c9309367cd7acf62ba";
          sha256 = "0p5im34yzj385vh05rz5kx4fflvib9vl4r64s9r2m7zzxl1zxida";
        };
      };
    };
    "phpactor/reference-finder" = {
      targetDir = "";
      src = composerEnv.buildZipPackage {
        name = "phpactor-reference-finder-754f5b12ce42c3e98cf7b331222b7b0f8158c85f";
        src = fetchurl {
          url = "https://api.github.com/repos/phpactor/reference-finder/zipball/754f5b12ce42c3e98cf7b331222b7b0f8158c85f";
          sha256 = "1fb80jdwkgzipzlq256fy2d6d42wkzzxvr7799zx1y9n92mrvwmd";
        };
      };
    };
    "phpactor/reference-finder-extension" = {
      targetDir = "";
      src = composerEnv.buildZipPackage {
        name = "phpactor-reference-finder-extension-f7126d074152351a5cfde3587a76892b372b017e";
        src = fetchurl {
          url = "https://api.github.com/repos/phpactor/reference-finder-extension/zipball/f7126d074152351a5cfde3587a76892b372b017e";
          sha256 = "0f5apg9hmifyfmk6hqspdljhr7kwm9xkc4fnwpm7xc374y2zh64x";
        };
      };
    };
    "phpactor/reference-finder-rpc-extension" = {
      targetDir = "";
      src = composerEnv.buildZipPackage {
        name = "phpactor-reference-finder-rpc-extension-181875da5c998b7a5144c2a46d8b4d6b8f8af454";
        src = fetchurl {
          url = "https://api.github.com/repos/phpactor/reference-finder-rpc-extension/zipball/181875da5c998b7a5144c2a46d8b4d6b8f8af454";
          sha256 = "0mjxx769znn47kkmxgx67bzf4z8846s415sy6j7xi0zc431s02xl";
        };
      };
    };
    "phpactor/rpc-extension" = {
      targetDir = "";
      src = composerEnv.buildZipPackage {
        name = "phpactor-rpc-extension-cfcde33c3f2821812d620283bb8116e88688309e";
        src = fetchurl {
          url = "https://api.github.com/repos/phpactor/rpc-extension/zipball/cfcde33c3f2821812d620283bb8116e88688309e";
          sha256 = "0dvw52p2ba2a9mbll93jahcffqki4y7bz66y4l949ia7xgf44lkc";
        };
      };
    };
    "phpactor/source-code-filesystem" = {
      targetDir = "";
      src = composerEnv.buildZipPackage {
        name = "phpactor-source-code-filesystem-4a5c527c624f590e579020aea5a11acf9fcdf760";
        src = fetchurl {
          url = "https://api.github.com/repos/phpactor/source-code-filesystem/zipball/4a5c527c624f590e579020aea5a11acf9fcdf760";
          sha256 = "180fx69jk5cyzqi5jkl9jwa01la6kv6w8l27zr5srjxaabv0v47j";
        };
      };
    };
    "phpactor/source-code-filesystem-extension" = {
      targetDir = "";
      src = composerEnv.buildZipPackage {
        name = "phpactor-source-code-filesystem-extension-bdfa6a5acc0eae2dc7bf4a10ac2226d060d35c5d";
        src = fetchurl {
          url = "https://api.github.com/repos/phpactor/source-code-filesystem-extension/zipball/bdfa6a5acc0eae2dc7bf4a10ac2226d060d35c5d";
          sha256 = "15la6sppsa0v5xr1vlq10yi6gq6cl0cqvk7gxyg7xl8kg83c0775";
        };
      };
    };
    "phpactor/text-document" = {
      targetDir = "";
      src = composerEnv.buildZipPackage {
        name = "phpactor-text-document-962070cbf33a63e87e30ba6c7617f1aa80fa2be4";
        src = fetchurl {
          url = "https://api.github.com/repos/phpactor/text-document/zipball/962070cbf33a63e87e30ba6c7617f1aa80fa2be4";
          sha256 = "1aib9km1nam9nmmxlcspalkgraxrxwd637ywv4rw69sps8rqdnqi";
        };
      };
    };
    "phpactor/worse-reference-finder-extension" = {
      targetDir = "";
      src = composerEnv.buildZipPackage {
        name = "phpactor-worse-reference-finder-extension-3b8b676634848e45c69bdcc8d4bed3ec851a01fc";
        src = fetchurl {
          url = "https://api.github.com/repos/phpactor/worse-reference-finder-extension/zipball/3b8b676634848e45c69bdcc8d4bed3ec851a01fc";
          sha256 = "18qk1hkx8zd9zx7bplxsfsnjxpmjn3rfbf9flkrlp4di7fiwjknc";
        };
      };
    };
    "phpactor/worse-reference-finders" = {
      targetDir = "";
      src = composerEnv.buildZipPackage {
        name = "phpactor-worse-reference-finders-2685e4e01062a00b3eb2f6f96b8ce6d89a2fd332";
        src = fetchurl {
          url = "https://api.github.com/repos/phpactor/worse-reference-finder/zipball/2685e4e01062a00b3eb2f6f96b8ce6d89a2fd332";
          sha256 = "00s144izakjl35sn8zrw8dkzy94m93c6rp7slypp7r4q29nxgnyx";
        };
      };
    };
    "phpactor/worse-reflection" = {
      targetDir = "";
      src = composerEnv.buildZipPackage {
        name = "phpactor-worse-reflection-4b4109af13c1815bfe3102ab0cf277bf9f231752";
        src = fetchurl {
          url = "https://api.github.com/repos/phpactor/worse-reflection/zipball/4b4109af13c1815bfe3102ab0cf277bf9f231752";
          sha256 = "0csc08m9gq69nwfivpyy8dzj6p8v4jwg8dxrzi6ha4nrn6ps8z31";
        };
      };
    };
    "phpactor/worse-reflection-extension" = {
      targetDir = "";
      src = composerEnv.buildZipPackage {
        name = "phpactor-worse-reflection-extension-7e626bb0ebcbb38bb52d3ffa71d59724feff83a4";
        src = fetchurl {
          url = "https://api.github.com/repos/phpactor/worse-reflection-extension/zipball/7e626bb0ebcbb38bb52d3ffa71d59724feff83a4";
          sha256 = "1pdnksz4a60sffgfcvjd29z4np4msvgq1lmncnc1j0p24svvvfai";
        };
      };
    };
    "psr/container" = {
      targetDir = "";
      src = composerEnv.buildZipPackage {
        name = "psr-container-8622567409010282b7aeebe4bb841fe98b58dcaf";
        src = fetchurl {
          url = "https://api.github.com/repos/php-fig/container/zipball/8622567409010282b7aeebe4bb841fe98b58dcaf";
          sha256 = "0qfvyfp3mli776kb9zda5cpc8cazj3prk0bg0gm254kwxyfkfrwn";
        };
      };
    };
    "psr/event-dispatcher" = {
      targetDir = "";
      src = composerEnv.buildZipPackage {
        name = "psr-event-dispatcher-aa4f89e91c423b516ff226c50dc83f824011c253";
        src = fetchurl {
          url = "https://api.github.com/repos/php-fig/event-dispatcher/zipball/aa4f89e91c423b516ff226c50dc83f824011c253";
          sha256 = "0j6linqfbscifa3ysx9jhxq3fz9ihlanpgn27qbiayhaxi1pbjhz";
        };
      };
    };
    "psr/log" = {
      targetDir = "";
      src = composerEnv.buildZipPackage {
        name = "psr-log-d49695b909c3b7628b6289db5479a1c204601f11";
        src = fetchurl {
          url = "https://api.github.com/repos/php-fig/log/zipball/d49695b909c3b7628b6289db5479a1c204601f11";
          sha256 = "0sb0mq30dvmzdgsnqvw3xh4fb4bqjncx72kf8n622f94dd48amln";
        };
      };
    };
    "ramsey/collection" = {
      targetDir = "";
      src = composerEnv.buildZipPackage {
        name = "ramsey-collection-eaca1dc1054ddd10cbd83c1461907bee6fb528fa";
        src = fetchurl {
          url = "https://api.github.com/repos/ramsey/collection/zipball/eaca1dc1054ddd10cbd83c1461907bee6fb528fa";
          sha256 = "0ix6m14jrcbia605kf3rddb1faszzv720nrq8qa02c6aw8jkk3i6";
        };
      };
    };
    "ramsey/uuid" = {
      targetDir = "";
      src = composerEnv.buildZipPackage {
        name = "ramsey-uuid-fe665a03df4f056aa65af552a96e1976df8c8dae";
        src = fetchurl {
          url = "https://api.github.com/repos/ramsey/uuid/zipball/fe665a03df4f056aa65af552a96e1976df8c8dae";
          sha256 = "1654nm0d7r9gq9gq0q5fs268xarxs80bhzx7rixyvcvik0djrqkr";
        };
      };
    };
    "sebastian/diff" = {
      targetDir = "";
      src = composerEnv.buildZipPackage {
        name = "sebastian-diff-3461e3fccc7cfdfc2720be910d3bd73c69be590d";
        src = fetchurl {
          url = "https://api.github.com/repos/sebastianbergmann/diff/zipball/3461e3fccc7cfdfc2720be910d3bd73c69be590d";
          sha256 = "0967nl6cdnr0v0z83w4xy59agn60kfv8gb41aw3fpy1n2wpp62dj";
        };
      };
    };
    "seld/jsonlint" = {
      targetDir = "";
      src = composerEnv.buildZipPackage {
        name = "seld-jsonlint-9ad6ce79c342fbd44df10ea95511a1b24dee5b57";
        src = fetchurl {
          url = "https://api.github.com/repos/Seldaek/jsonlint/zipball/9ad6ce79c342fbd44df10ea95511a1b24dee5b57";
          sha256 = "1ywni3i7zi2bsh7qpbf710qixd3jhpvz4l1bavrw9vnkxl38qj8p";
        };
      };
    };
    "seld/phar-utils" = {
      targetDir = "";
      src = composerEnv.buildZipPackage {
        name = "seld-phar-utils-749042a2315705d2dfbbc59234dd9ceb22bf3ff0";
        src = fetchurl {
          url = "https://api.github.com/repos/Seldaek/phar-utils/zipball/749042a2315705d2dfbbc59234dd9ceb22bf3ff0";
          sha256 = "12m1ql9kmz3jhdc6k55fin191v1qyvdmisk3m10kxljh4nvyiin6";
        };
      };
    };
    "symfony/console" = {
      targetDir = "";
      src = composerEnv.buildZipPackage {
        name = "symfony-console-e4ad98844987793bd60d017cc3854b7758f47e55";
        src = fetchurl {
          url = "https://api.github.com/repos/symfony/console/zipball/e4ad98844987793bd60d017cc3854b7758f47e55";
          sha256 = "0bkac23wbpj5z2gw14x0ps3cc5x5b8y6n8wvjb0sw9ll2ibp8nmb";
        };
      };
    };
    "symfony/deprecation-contracts" = {
      targetDir = "";
      src = composerEnv.buildZipPackage {
        name = "symfony-deprecation-contracts-6f981ee24cf69ee7ce9736146d1c57c2780598a8";
        src = fetchurl {
          url = "https://api.github.com/repos/symfony/deprecation-contracts/zipball/6f981ee24cf69ee7ce9736146d1c57c2780598a8";
          sha256 = "05jws1g4kcs297bwf5d72z47m2263i2jqpivi3yv8kf50kdjjzba";
        };
      };
    };
    "symfony/filesystem" = {
      targetDir = "";
      src = composerEnv.buildZipPackage {
        name = "symfony-filesystem-2d9fb8108eb8447f5a3189d666c54a242240e67e";
        src = fetchurl {
          url = "https://api.github.com/repos/symfony/filesystem/zipball/2d9fb8108eb8447f5a3189d666c54a242240e67e";
          sha256 = "1wj05s6115n1273fy1ccqjg966vdvxamiv7rh8ank7nrlqkzxigq";
        };
      };
    };
    "symfony/finder" = {
      targetDir = "";
      src = composerEnv.buildZipPackage {
        name = "symfony-finder-982b4a1044ec54a2ad55c8001bf66dbdb3e3c4c1";
        src = fetchurl {
          url = "https://api.github.com/repos/symfony/finder/zipball/982b4a1044ec54a2ad55c8001bf66dbdb3e3c4c1";
          sha256 = "1sf51mjv1rq3pgrhdqncmak1l1i9g7gad1n5w8zd0lkkg2r79d87";
        };
      };
    };
    "symfony/polyfill-ctype" = {
      targetDir = "";
      src = composerEnv.buildZipPackage {
        name = "symfony-polyfill-ctype-46cd95797e9df938fdd2b03693b5fca5e64b01ce";
        src = fetchurl {
          url = "https://api.github.com/repos/symfony/polyfill-ctype/zipball/46cd95797e9df938fdd2b03693b5fca5e64b01ce";
          sha256 = "0z4iiznxxs4r72xs4irqqb6c0wnwpwf0hklwn2imls67haq330zn";
        };
      };
    };
    "symfony/polyfill-intl-grapheme" = {
      targetDir = "";
      src = composerEnv.buildZipPackage {
        name = "symfony-polyfill-intl-grapheme-16880ba9c5ebe3642d1995ab866db29270b36535";
        src = fetchurl {
          url = "https://api.github.com/repos/symfony/polyfill-intl-grapheme/zipball/16880ba9c5ebe3642d1995ab866db29270b36535";
          sha256 = "0pb57756kvdxksqy2nndf8q7c91p2dzhysa52x2rbhba869760fv";
        };
      };
    };
    "symfony/polyfill-intl-normalizer" = {
      targetDir = "";
      src = composerEnv.buildZipPackage {
        name = "symfony-polyfill-intl-normalizer-8590a5f561694770bdcd3f9b5c69dde6945028e8";
        src = fetchurl {
          url = "https://api.github.com/repos/symfony/polyfill-intl-normalizer/zipball/8590a5f561694770bdcd3f9b5c69dde6945028e8";
          sha256 = "1c60xin00q0d2gbyaiglxppn5hqwki616v5chzwyhlhf6aplwsh3";
        };
      };
    };
    "symfony/polyfill-mbstring" = {
      targetDir = "";
      src = composerEnv.buildZipPackage {
        name = "symfony-polyfill-mbstring-9174a3d80210dca8daa7f31fec659150bbeabfc6";
        src = fetchurl {
          url = "https://api.github.com/repos/symfony/polyfill-mbstring/zipball/9174a3d80210dca8daa7f31fec659150bbeabfc6";
          sha256 = "17bhba3093di6xgi8f0cnf3cdd7fnbyp9l76d9y33cym6213ayx1";
        };
      };
    };
    "symfony/polyfill-php73" = {
      targetDir = "";
      src = composerEnv.buildZipPackage {
        name = "symfony-polyfill-php73-fba8933c384d6476ab14fb7b8526e5287ca7e010";
        src = fetchurl {
          url = "https://api.github.com/repos/symfony/polyfill-php73/zipball/fba8933c384d6476ab14fb7b8526e5287ca7e010";
          sha256 = "0fc1d60iw8iar2zcvkzwdvx0whkbw8p6ll0cry39nbkklzw85n1h";
        };
      };
    };
    "symfony/polyfill-php80" = {
      targetDir = "";
      src = composerEnv.buildZipPackage {
        name = "symfony-polyfill-php80-1100343ed1a92e3a38f9ae122fc0eb21602547be";
        src = fetchurl {
          url = "https://api.github.com/repos/symfony/polyfill-php80/zipball/1100343ed1a92e3a38f9ae122fc0eb21602547be";
          sha256 = "0kwk2qgwswsmbfp1qx31ahw3lisgyivwhw5dycshr5v2iwwx3rhi";
        };
      };
    };
    "symfony/polyfill-php81" = {
      targetDir = "";
      src = composerEnv.buildZipPackage {
        name = "symfony-polyfill-php81-e66119f3de95efc359483f810c4c3e6436279436";
        src = fetchurl {
          url = "https://api.github.com/repos/symfony/polyfill-php81/zipball/e66119f3de95efc359483f810c4c3e6436279436";
          sha256 = "0hg340da7m0yipj2bj5hxhd3mqidz767ivg7w85r8vwz3mr9k1p3";
        };
      };
    };
    "symfony/process" = {
      targetDir = "";
      src = composerEnv.buildZipPackage {
        name = "symfony-process-a3ab47968ef2cc5124efc4f1b6b4e7a61b85a543";
        src = fetchurl {
          url = "https://api.github.com/repos/symfony/process/zipball/a3ab47968ef2cc5124efc4f1b6b4e7a61b85a543";
          sha256 = "1xricmrpwpmx5prmkcylcrkdypsr0qr5r98gzh9nd06y009fnig9";
        };
      };
    };
    "symfony/service-contracts" = {
      targetDir = "";
      src = composerEnv.buildZipPackage {
        name = "symfony-service-contracts-56b990c18120c91eaf0d38a93fabfa2a1f7fa413";
        src = fetchurl {
          url = "https://api.github.com/repos/symfony/service-contracts/zipball/56b990c18120c91eaf0d38a93fabfa2a1f7fa413";
          sha256 = "0xnizqglqbwqpiwff864a0cmjqn3n9v0sp5y7hv0qwpa2v1sx6ir";
        };
      };
    };
    "symfony/string" = {
      targetDir = "";
      src = composerEnv.buildZipPackage {
        name = "symfony-string-fa2c5cc3f7dac23d87429652fe0daf28d65cbd5b";
        src = fetchurl {
          url = "https://api.github.com/repos/symfony/string/zipball/fa2c5cc3f7dac23d87429652fe0daf28d65cbd5b";
          sha256 = "03y78mqq0aqqqs6vr9akawq2q5gj82c662m39lx2qni8cdspsbss";
        };
      };
    };
    "symfony/yaml" = {
      targetDir = "";
      src = composerEnv.buildZipPackage {
        name = "symfony-yaml-9cb82982ab2268c1336a6056e9e2068efdcdce9d";
        src = fetchurl {
          url = "https://api.github.com/repos/symfony/yaml/zipball/9cb82982ab2268c1336a6056e9e2068efdcdce9d";
          sha256 = "06plmr91r05iaxc9snnq66dn1x6cy53ni1r3p8ycswhink81wjb7";
        };
      };
    };
    "thecodingmachine/safe" = {
      targetDir = "";
      src = composerEnv.buildZipPackage {
        name = "thecodingmachine-safe-9f277171e296a3c8629c04ac93ec95ff0f208ccb";
        src = fetchurl {
          url = "https://api.github.com/repos/thecodingmachine/safe/zipball/9f277171e296a3c8629c04ac93ec95ff0f208ccb";
          sha256 = "0qslks98ywlmiyyskvh6mrphd6d45qln6jyaviz5lrfck2lwy0xm";
        };
      };
    };
    "twig/twig" = {
      targetDir = "";
      src = composerEnv.buildZipPackage {
        name = "twig-twig-50a41e48d0e3571a5fc4c031eef4458a82a69f12";
        src = fetchurl {
          url = "https://api.github.com/repos/twigphp/Twig/zipball/50a41e48d0e3571a5fc4c031eef4458a82a69f12";
          sha256 = "073jz85kp5lab4gdzja1hj1is2zzliz9b9w874gsmym01dfxmnqb";
        };
      };
    };
    "webmozart/assert" = {
      targetDir = "";
      src = composerEnv.buildZipPackage {
        name = "webmozart-assert-b419d648592b0b8911cbbe10d450fe314f4fd262";
        src = fetchurl {
          url = "https://api.github.com/repos/webmozarts/assert/zipball/b419d648592b0b8911cbbe10d450fe314f4fd262";
          sha256 = "0pivq0m5s52hcxjb8vzs4d4bhd14jm7gsv98672q5ix9iccg9g4c";
        };
      };
    };
    "webmozart/glob" = {
      targetDir = "";
      src = composerEnv.buildZipPackage {
        name = "webmozart-glob-36c35b0a51eece57a6a631decb51048e17192401";
        src = fetchurl {
          url = "https://api.github.com/repos/webmozarts/glob/zipball/36c35b0a51eece57a6a631decb51048e17192401";
          sha256 = "0xacbyvpharys5fjbcgafrv6piqwf5dxc57nxbg359qgvwn7fvhw";
        };
      };
    };
    "webmozart/path-util" = {
      targetDir = "";
      src = composerEnv.buildZipPackage {
        name = "webmozart-path-util-95a8f7ad150c2a3773ff3c3d04f557a24c99cfd2";
        src = fetchurl {
          url = "https://api.github.com/repos/webmozart/path-util/zipball/95a8f7ad150c2a3773ff3c3d04f557a24c99cfd2";
          sha256 = "1n3gbajvawyxm0zxdrgncfg5fmxnp3xc8fwdqmv3yq59fdfba8s0";
        };
      };
    };
  };
  devPackages = {};
in
  composerEnv.buildPackage {
    inherit packages devPackages noDev;
    name = "phpactor-phpactor";
    src = ./.;
    executable = true;
    symlinkDependencies = false;
    meta = {};
  }
