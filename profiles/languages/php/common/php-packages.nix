{composerEnv, fetchurl, fetchgit ? null, fetchhg ? null, fetchsvn ? null, noDev ? false}:

let
  packages = {
    "composer/semver" = {
      targetDir = "";
      src = composerEnv.buildZipPackage {
        name = "composer-semver-31f3ea725711245195f62e54ffa402d8ef2fdba9";
        src = fetchurl {
          url = "https://api.github.com/repos/composer/semver/zipball/31f3ea725711245195f62e54ffa402d8ef2fdba9";
          sha256 = "13f1hcrq2bn8b43562p7izq7qkcj8nn2apiiwmwybichm5lgcx70";
        };
      };
    };
    "composer/xdebug-handler" = {
      targetDir = "";
      src = composerEnv.buildZipPackage {
        name = "composer-xdebug-handler-84674dd3a7575ba617f5a76d7e9e29a7d3891339";
        src = fetchurl {
          url = "https://api.github.com/repos/composer/xdebug-handler/zipball/84674dd3a7575ba617f5a76d7e9e29a7d3891339";
          sha256 = "07cqkz9b2bc3vfnm8xg4j83szfkk8q3azy5ahhl14i2k560x65f4";
        };
      };
    };
    "d11wtq/boris" = {
      targetDir = "";
      src = composerEnv.buildZipPackage {
        name = "d11wtq-boris-793a5f196bc47b93ed95c5dcdacee52c7b3536fc";
        src = fetchurl {
          url = "https://api.github.com/repos/borisrepl/boris/zipball/793a5f196bc47b93ed95c5dcdacee52c7b3536fc";
          sha256 = "0z96n1ibxlxwig7pvyhk00i3zcsqrav1fm2ppagr2r0v1f5ahw6f";
        };
      };
    };
    "doctrine/annotations" = {
      targetDir = "";
      src = composerEnv.buildZipPackage {
        name = "doctrine-annotations-732ea120f89e570dd63828b91af83a1b48e21dfb";
        src = fetchurl {
          url = "https://api.github.com/repos/doctrine/annotations/zipball/732ea120f89e570dd63828b91af83a1b48e21dfb";
          sha256 = "0i698w0wpfm6gszwgjfv48wvssp746rj78zvgd1idycnl34k654g";
        };
      };
    };
    "doctrine/instantiator" = {
      targetDir = "";
      src = composerEnv.buildZipPackage {
        name = "doctrine-instantiator-6410c4b8352cb64218641457cef64997e6b784fb";
        src = fetchurl {
          url = "https://api.github.com/repos/doctrine/instantiator/zipball/6410c4b8352cb64218641457cef64997e6b784fb";
          sha256 = "0bgzdlamxy8ayafhgj167jxxipraglx95s5dfh9jqj53k0b0fz9i";
        };
      };
    };
    "doctrine/lexer" = {
      targetDir = "";
      src = composerEnv.buildZipPackage {
        name = "doctrine-lexer-59bfb3b9be04237be4cd1afea9bbb58794c25ce8";
        src = fetchurl {
          url = "https://api.github.com/repos/doctrine/lexer/zipball/59bfb3b9be04237be4cd1afea9bbb58794c25ce8";
          sha256 = "0mp8d0ylw4bq3hp8nkfd7cmxs858cv6l8hbdvs07ygvxdw3il189";
        };
      };
    };
    "friendsofphp/php-cs-fixer" = {
      targetDir = "";
      src = composerEnv.buildZipPackage {
        name = "friendsofphp-php-cs-fixer-beea139824be94ab455cd9b6612ace0e388c931b";
        src = fetchurl {
          url = "https://api.github.com/repos/FriendsOfPHP/PHP-CS-Fixer/zipball/beea139824be94ab455cd9b6612ace0e388c931b";
          sha256 = "1lpn2pbgmkmmidqv71p628b3xha15givddpb0dcs700ddr46f259";
        };
      };
    };
    "myclabs/deep-copy" = {
      targetDir = "";
      src = composerEnv.buildZipPackage {
        name = "myclabs-deep-copy-776f831124e9c62e1a2c601ecc52e776d8bb7220";
        src = fetchurl {
          url = "https://api.github.com/repos/myclabs/DeepCopy/zipball/776f831124e9c62e1a2c601ecc52e776d8bb7220";
          sha256 = "181f3fsxs6s2wyy4y7qfk08qmlbvz1wn3mn3lqy42grsb8g8ym0k";
        };
      };
    };
    "nikic/php-parser" = {
      targetDir = "";
      src = composerEnv.buildZipPackage {
        name = "nikic-php-parser-50953a2691a922aa1769461637869a0a2faa3f53";
        src = fetchurl {
          url = "https://api.github.com/repos/nikic/PHP-Parser/zipball/50953a2691a922aa1769461637869a0a2faa3f53";
          sha256 = "1mkl7lbvyxs7z8lh4p3i0j296hvzslrvwbf9cjhb2qhncsxxqrz6";
        };
      };
    };
    "phar-io/manifest" = {
      targetDir = "";
      src = composerEnv.buildZipPackage {
        name = "phar-io-manifest-97803eca37d319dfa7826cc2437fc020857acb53";
        src = fetchurl {
          url = "https://api.github.com/repos/phar-io/manifest/zipball/97803eca37d319dfa7826cc2437fc020857acb53";
          sha256 = "107dsj04ckswywc84dvw42kdrqd4y6yvb2qwacigyrn05p075c1w";
        };
      };
    };
    "phar-io/version" = {
      targetDir = "";
      src = composerEnv.buildZipPackage {
        name = "phar-io-version-bae7c545bef187884426f042434e561ab1ddb182";
        src = fetchurl {
          url = "https://api.github.com/repos/phar-io/version/zipball/bae7c545bef187884426f042434e561ab1ddb182";
          sha256 = "0hqmrihb4wv53rl3fg93wjldwrz79jyad5bv29ynbdklsirh7b2l";
        };
      };
    };
    "php-cs-fixer/diff" = {
      targetDir = "";
      src = composerEnv.buildZipPackage {
        name = "php-cs-fixer-diff-29dc0d507e838c4580d018bd8b5cb412474f7ec3";
        src = fetchurl {
          url = "https://api.github.com/repos/PHP-CS-Fixer/diff/zipball/29dc0d507e838c4580d018bd8b5cb412474f7ec3";
          sha256 = "12b0ga9i0racym4vvql26kjjiqx2940j0345kmy9zjbamm6jzlzl";
        };
      };
    };
    "phpdocumentor/reflection-common" = {
      targetDir = "";
      src = composerEnv.buildZipPackage {
        name = "phpdocumentor-reflection-common-a0eeab580cbdf4414fef6978732510a36ed0a9d6";
        src = fetchurl {
          url = "https://api.github.com/repos/phpDocumentor/ReflectionCommon/zipball/a0eeab580cbdf4414fef6978732510a36ed0a9d6";
          sha256 = "1ixvhcacxlxs2ypyvhfyf4zpm32qy59jy55cw3wqrl53ri7f2lb3";
        };
      };
    };
    "phpdocumentor/reflection-docblock" = {
      targetDir = "";
      src = composerEnv.buildZipPackage {
        name = "phpdocumentor-reflection-docblock-0f36fa483bc30d0032f700a38b40141cfa383910";
        src = fetchurl {
          url = "https://api.github.com/repos/phpDocumentor/ReflectionDocBlock/zipball/0f36fa483bc30d0032f700a38b40141cfa383910";
          sha256 = "0qc57x0snxqpjpc8qig9k4bjlnycrxqn1ib6r8pjlzq6l61ld5w6";
        };
      };
    };
    "phpdocumentor/type-resolver" = {
      targetDir = "";
      src = composerEnv.buildZipPackage {
        name = "phpdocumentor-type-resolver-a12f7e301eb7258bb68acd89d4aefa05c2906cae";
        src = fetchurl {
          url = "https://api.github.com/repos/phpDocumentor/TypeResolver/zipball/a12f7e301eb7258bb68acd89d4aefa05c2906cae";
          sha256 = "1kziz1qkq15d4gbxqpv8s5sy1bfd11djsvyqn27dcqx6rx0b3pkm";
        };
      };
    };
    "phpspec/prophecy" = {
      targetDir = "";
      src = composerEnv.buildZipPackage {
        name = "phpspec-prophecy-d86dfc2e2a3cd366cee475e52c6bb3bbc371aa0e";
        src = fetchurl {
          url = "https://api.github.com/repos/phpspec/prophecy/zipball/d86dfc2e2a3cd366cee475e52c6bb3bbc371aa0e";
          sha256 = "1v61xv4jg9sqkfkc52p8hca2283b1h7zkajqg4dfb1k78cqxr4js";
        };
      };
    };
    "phpunit/php-code-coverage" = {
      targetDir = "";
      src = composerEnv.buildZipPackage {
        name = "phpunit-php-code-coverage-3ca2f862e11bb02cb7a97185afed902522e2edbb";
        src = fetchurl {
          url = "https://api.github.com/repos/sebastianbergmann/php-code-coverage/zipball/3ca2f862e11bb02cb7a97185afed902522e2edbb";
          sha256 = "1y3wh4cw0vhkcw3pfhj3rz36q43pnf78x88yk460hbzk9pw6q7x7";
        };
      };
    };
    "phpunit/php-file-iterator" = {
      targetDir = "";
      src = composerEnv.buildZipPackage {
        name = "phpunit-php-file-iterator-d7e633e95043246c5370e96d4cd17aa2cc79ab78";
        src = fetchurl {
          url = "https://api.github.com/repos/sebastianbergmann/php-file-iterator/zipball/d7e633e95043246c5370e96d4cd17aa2cc79ab78";
          sha256 = "1gym93s6rvvfl5535qrpmsm0xgjrq721m6y84k3a978mqr04apca";
        };
      };
    };
    "phpunit/php-invoker" = {
      targetDir = "";
      src = composerEnv.buildZipPackage {
        name = "phpunit-php-invoker-5a10147d0aaf65b58940a0b72f71c9ac0423cc67";
        src = fetchurl {
          url = "https://api.github.com/repos/sebastianbergmann/php-invoker/zipball/5a10147d0aaf65b58940a0b72f71c9ac0423cc67";
          sha256 = "1vqnnjnw94mzm30n9n5p2bfgd3wd5jah92q6cj3gz1nf0qigr4fh";
        };
      };
    };
    "phpunit/php-text-template" = {
      targetDir = "";
      src = composerEnv.buildZipPackage {
        name = "phpunit-php-text-template-5da5f67fc95621df9ff4c4e5a84d6a8a2acf7c28";
        src = fetchurl {
          url = "https://api.github.com/repos/sebastianbergmann/php-text-template/zipball/5da5f67fc95621df9ff4c4e5a84d6a8a2acf7c28";
          sha256 = "0ff87yzywizi6j2ps3w0nalpx16mfyw3imzn6gj9jjsfwc2bb8lq";
        };
      };
    };
    "phpunit/php-timer" = {
      targetDir = "";
      src = composerEnv.buildZipPackage {
        name = "phpunit-php-timer-5a63ce20ed1b5bf577850e2c4e87f4aa902afbd2";
        src = fetchurl {
          url = "https://api.github.com/repos/sebastianbergmann/php-timer/zipball/5a63ce20ed1b5bf577850e2c4e87f4aa902afbd2";
          sha256 = "0g1g7yy4zk1bidyh165fsbqx5y8f1c8pxikvcahzlfsr9p2qxk6a";
        };
      };
    };
    "phpunit/phpunit" = {
      targetDir = "";
      src = composerEnv.buildZipPackage {
        name = "phpunit-phpunit-c814a05837f2edb0d1471d6e3f4ab3501ca3899a";
        src = fetchurl {
          url = "https://api.github.com/repos/sebastianbergmann/phpunit/zipball/c814a05837f2edb0d1471d6e3f4ab3501ca3899a";
          sha256 = "1lj8l30c6pqn9jlbzkb1id7avpwf2icsqcav2zgxc8jrf1lp1wfz";
        };
      };
    };
    "psr/cache" = {
      targetDir = "";
      src = composerEnv.buildZipPackage {
        name = "psr-cache-d11b50ad223250cf17b86e38383413f5a6764bf8";
        src = fetchurl {
          url = "https://api.github.com/repos/php-fig/cache/zipball/d11b50ad223250cf17b86e38383413f5a6764bf8";
          sha256 = "06i2k3dx3b4lgn9a4v1dlgv8l9wcl4kl7vzhh63lbji0q96hv8qz";
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
    "sebastian/cli-parser" = {
      targetDir = "";
      src = composerEnv.buildZipPackage {
        name = "sebastian-cli-parser-442e7c7e687e42adc03470c7b668bc4b2402c0b2";
        src = fetchurl {
          url = "https://api.github.com/repos/sebastianbergmann/cli-parser/zipball/442e7c7e687e42adc03470c7b668bc4b2402c0b2";
          sha256 = "074qzdq19k9x4svhq3nak5h348xska56v1sqnhk1aj0jnrx02h37";
        };
      };
    };
    "sebastian/code-unit" = {
      targetDir = "";
      src = composerEnv.buildZipPackage {
        name = "sebastian-code-unit-1fc9f64c0927627ef78ba436c9b17d967e68e120";
        src = fetchurl {
          url = "https://api.github.com/repos/sebastianbergmann/code-unit/zipball/1fc9f64c0927627ef78ba436c9b17d967e68e120";
          sha256 = "04vlx050rrd54mxal7d93pz4119pas17w3gg5h532anfxjw8j7pm";
        };
      };
    };
    "sebastian/code-unit-reverse-lookup" = {
      targetDir = "";
      src = composerEnv.buildZipPackage {
        name = "sebastian-code-unit-reverse-lookup-ac91f01ccec49fb77bdc6fd1e548bc70f7faa3e5";
        src = fetchurl {
          url = "https://api.github.com/repos/sebastianbergmann/code-unit-reverse-lookup/zipball/ac91f01ccec49fb77bdc6fd1e548bc70f7faa3e5";
          sha256 = "1h1jbzz3zak19qi4mab2yd0ddblpz7p000jfyxfwd2ds0gmrnsja";
        };
      };
    };
    "sebastian/comparator" = {
      targetDir = "";
      src = composerEnv.buildZipPackage {
        name = "sebastian-comparator-55f4261989e546dc112258c7a75935a81a7ce382";
        src = fetchurl {
          url = "https://api.github.com/repos/sebastianbergmann/comparator/zipball/55f4261989e546dc112258c7a75935a81a7ce382";
          sha256 = "1d4bgf4m2x0kn3nw9hbb45asbx22lsp9vxl74rp1yl3sj2vk9sch";
        };
      };
    };
    "sebastian/complexity" = {
      targetDir = "";
      src = composerEnv.buildZipPackage {
        name = "sebastian-complexity-739b35e53379900cc9ac327b2147867b8b6efd88";
        src = fetchurl {
          url = "https://api.github.com/repos/sebastianbergmann/complexity/zipball/739b35e53379900cc9ac327b2147867b8b6efd88";
          sha256 = "1y4yz8n8hszbhinf9ipx3pqyvgm7gz0krgyn19z0097yq3bbq8yf";
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
    "sebastian/environment" = {
      targetDir = "";
      src = composerEnv.buildZipPackage {
        name = "sebastian-environment-388b6ced16caa751030f6a69e588299fa09200ac";
        src = fetchurl {
          url = "https://api.github.com/repos/sebastianbergmann/environment/zipball/388b6ced16caa751030f6a69e588299fa09200ac";
          sha256 = "022vn8zss3sm7hg83kg3y0lmjw2ak6cy64b584nbsgxfhlmf6msd";
        };
      };
    };
    "sebastian/exporter" = {
      targetDir = "";
      src = composerEnv.buildZipPackage {
        name = "sebastian-exporter-d89cc98761b8cb5a1a235a6b703ae50d34080e65";
        src = fetchurl {
          url = "https://api.github.com/repos/sebastianbergmann/exporter/zipball/d89cc98761b8cb5a1a235a6b703ae50d34080e65";
          sha256 = "1s8v0cbcjdb0wvwyh869y5f8d55mpjkr0f3gg2kvvxk3wh8nvvc7";
        };
      };
    };
    "sebastian/global-state" = {
      targetDir = "";
      src = composerEnv.buildZipPackage {
        name = "sebastian-global-state-23bd5951f7ff26f12d4e3242864df3e08dec4e49";
        src = fetchurl {
          url = "https://api.github.com/repos/sebastianbergmann/global-state/zipball/23bd5951f7ff26f12d4e3242864df3e08dec4e49";
          sha256 = "0hrh5knc2g5i288kh9lkwmr6hb7yav5m8i21piz8pfh7kc75czjw";
        };
      };
    };
    "sebastian/lines-of-code" = {
      targetDir = "";
      src = composerEnv.buildZipPackage {
        name = "sebastian-lines-of-code-c1c2e997aa3146983ed888ad08b15470a2e22ecc";
        src = fetchurl {
          url = "https://api.github.com/repos/sebastianbergmann/lines-of-code/zipball/c1c2e997aa3146983ed888ad08b15470a2e22ecc";
          sha256 = "0fay9s5cm16gbwr7qjihwrzxn7sikiwba0gvda16xng903argbk0";
        };
      };
    };
    "sebastian/object-enumerator" = {
      targetDir = "";
      src = composerEnv.buildZipPackage {
        name = "sebastian-object-enumerator-5c9eeac41b290a3712d88851518825ad78f45c71";
        src = fetchurl {
          url = "https://api.github.com/repos/sebastianbergmann/object-enumerator/zipball/5c9eeac41b290a3712d88851518825ad78f45c71";
          sha256 = "11853z07w8h1a67wsjy3a6ir5x7khgx6iw5bmrkhjkiyvandqcn1";
        };
      };
    };
    "sebastian/object-reflector" = {
      targetDir = "";
      src = composerEnv.buildZipPackage {
        name = "sebastian-object-reflector-b4f479ebdbf63ac605d183ece17d8d7fe49c15c7";
        src = fetchurl {
          url = "https://api.github.com/repos/sebastianbergmann/object-reflector/zipball/b4f479ebdbf63ac605d183ece17d8d7fe49c15c7";
          sha256 = "0g5m1fswy6wlf300x1vcipjdljmd3vh05hjqhqfc91byrjbk4rsg";
        };
      };
    };
    "sebastian/recursion-context" = {
      targetDir = "";
      src = composerEnv.buildZipPackage {
        name = "sebastian-recursion-context-cd9d8cf3c5804de4341c283ed787f099f5506172";
        src = fetchurl {
          url = "https://api.github.com/repos/sebastianbergmann/recursion-context/zipball/cd9d8cf3c5804de4341c283ed787f099f5506172";
          sha256 = "1k0ki1krwq6329vsbw3515wsyg8a7n2l83lk19pdc12i2lg9nhpy";
        };
      };
    };
    "sebastian/resource-operations" = {
      targetDir = "";
      src = composerEnv.buildZipPackage {
        name = "sebastian-resource-operations-0f4443cb3a1d92ce809899753bc0d5d5a8dd19a8";
        src = fetchurl {
          url = "https://api.github.com/repos/sebastianbergmann/resource-operations/zipball/0f4443cb3a1d92ce809899753bc0d5d5a8dd19a8";
          sha256 = "0p5s8rp7mrhw20yz5wx1i4k8ywf0h0ximcqan39n9qnma1dlnbyr";
        };
      };
    };
    "sebastian/type" = {
      targetDir = "";
      src = composerEnv.buildZipPackage {
        name = "sebastian-type-f24cbc541026c3bb7d27c647f0f9ea337135b22a";
        src = fetchurl {
          url = "https://api.github.com/repos/sebastianbergmann/type/zipball/f24cbc541026c3bb7d27c647f0f9ea337135b22a";
          sha256 = "0h9k8ykkx7z15ny1kb4994sy7h4bp51zrpj8d00f1vzwl3kxihkw";
        };
      };
    };
    "sebastian/version" = {
      targetDir = "";
      src = composerEnv.buildZipPackage {
        name = "sebastian-version-c6c1022351a901512170118436c764e473f6de8c";
        src = fetchurl {
          url = "https://api.github.com/repos/sebastianbergmann/version/zipball/c6c1022351a901512170118436c764e473f6de8c";
          sha256 = "1bs7bwa9m0fin1zdk7vqy5lxzlfa9la90lkl27sn0wr00m745ig1";
        };
      };
    };
    "squizlabs/php_codesniffer" = {
      targetDir = "";
      src = composerEnv.buildZipPackage {
        name = "squizlabs-php_codesniffer-56873a53dbe502db3aa160043f8a595b2c192181";
        src = fetchurl {
          url = "https://api.github.com/repos/squizlabs/PHP_CodeSniffer/zipball/56873a53dbe502db3aa160043f8a595b2c192181";
          sha256 = "1i5my63zadyr8z6ldn8xpmnkiqks6jb12frw7asq2fqlz5pws7x1";
        };
      };
    };
    "svanderburg/composer2nix" = {
      targetDir = "";
      src = composerEnv.buildZipPackage {
        name = "svanderburg-composer2nix-9983c6fafb277f6b305edf232c2690d6d28ec092";
        src = fetchurl {
          url = "https://api.github.com/repos/svanderburg/composer2nix/zipball/9983c6fafb277f6b305edf232c2690d6d28ec092";
          sha256 = "1s1gv2b4y9pjv56mif8fgch56sssdmrcwb1gk3gksxc0jq2w2zbv";
        };
      };
    };
    "svanderburg/pndp" = {
      targetDir = "";
      src = composerEnv.buildZipPackage {
        name = "svanderburg-pndp-4bfe9c4120c23354ab8dc295957dc3009a39bff0";
        src = fetchurl {
          url = "https://api.github.com/repos/svanderburg/pndp/zipball/4bfe9c4120c23354ab8dc295957dc3009a39bff0";
          sha256 = "0n2vwpwshv16bhb7a6j95m664zh4lpfa7dqmcyhmn89nxpgvg91y";
        };
      };
    };
    "symfony/console" = {
      targetDir = "";
      src = composerEnv.buildZipPackage {
        name = "symfony-console-9ca59c3e9eb6c46e16c8a1129bb3cffb314e86fa";
        src = fetchurl {
          url = "https://api.github.com/repos/symfony/console/zipball/9ca59c3e9eb6c46e16c8a1129bb3cffb314e86fa";
          sha256 = "05xlzylhp2h90hv35gqdlp6zdfzk2ywgzcjv6d71zbzcivzl9rcz";
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
    "symfony/event-dispatcher" = {
      targetDir = "";
      src = composerEnv.buildZipPackage {
        name = "symfony-event-dispatcher-ac2f062e33c4bb8338cf01fd981baed0028f46cb";
        src = fetchurl {
          url = "https://api.github.com/repos/symfony/event-dispatcher/zipball/ac2f062e33c4bb8338cf01fd981baed0028f46cb";
          sha256 = "0zddpl5qzgm4wl3p5hwrs902g7637z6mj73g0bx7sk624fkj17yb";
        };
      };
    };
    "symfony/event-dispatcher-contracts" = {
      targetDir = "";
      src = composerEnv.buildZipPackage {
        name = "symfony-event-dispatcher-contracts-66bea3b09be61613cd3b4043a65a8ec48cfa6d2a";
        src = fetchurl {
          url = "https://api.github.com/repos/symfony/event-dispatcher-contracts/zipball/66bea3b09be61613cd3b4043a65a8ec48cfa6d2a";
          sha256 = "03bx5j7xh5bv1v17nlaw9wnbad66bzwp5w7npg8w2b01my49phfy";
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
        name = "symfony-finder-76e5ba5e89f7951e12ef5220999f51268ad8af58";
        src = fetchurl {
          url = "https://api.github.com/repos/symfony/finder/zipball/76e5ba5e89f7951e12ef5220999f51268ad8af58";
          sha256 = "0j06zxwf1fsdqpqizm7jxqgp5yp37kzzanxfx0wng4v1qwg7y3bk";
        };
      };
    };
    "symfony/options-resolver" = {
      targetDir = "";
      src = composerEnv.buildZipPackage {
        name = "symfony-options-resolver-cd63dbab0428a47f8576e4e58148aeae2e32e91c";
        src = fetchurl {
          url = "https://api.github.com/repos/symfony/options-resolver/zipball/cd63dbab0428a47f8576e4e58148aeae2e32e91c";
          sha256 = "1k942x9wz67h9sjf3wifr9245dlp3q5db7qkb3h19ri9s0hgjhpy";
        };
      };
    };
    "symfony/polyfill-ctype" = {
      targetDir = "";
      src = composerEnv.buildZipPackage {
        name = "symfony-polyfill-ctype-f24ae462b1d60c333df104f0b81ec7d0e12f6e9f";
        src = fetchurl {
          url = "https://api.github.com/repos/symfony/polyfill-ctype/zipball/f24ae462b1d60c333df104f0b81ec7d0e12f6e9f";
          sha256 = "0f168rhmpgd59lzjw7vh9izwj8crxm2dvb2n67f6qs57aq1lyzqa";
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
        name = "symfony-polyfill-mbstring-344e456152e22a1bce3048c6c311059ea14bde47";
        src = fetchurl {
          url = "https://api.github.com/repos/symfony/polyfill-mbstring/zipball/344e456152e22a1bce3048c6c311059ea14bde47";
          sha256 = "002h72bnz2xkn882kiymw2yscbd93dw6arhhsfz2azlr782qxcsv";
        };
      };
    };
    "symfony/polyfill-php72" = {
      targetDir = "";
      src = composerEnv.buildZipPackage {
        name = "symfony-polyfill-php72-9a142215a36a3888e30d0a9eeea9766764e96976";
        src = fetchurl {
          url = "https://api.github.com/repos/symfony/polyfill-php72/zipball/9a142215a36a3888e30d0a9eeea9766764e96976";
          sha256 = "06ipbcvrxjzgvraf2z9fwgy0bzvzjvs5z1j67grg1gb15x3d428b";
        };
      };
    };
    "symfony/polyfill-php73" = {
      targetDir = "";
      src = composerEnv.buildZipPackage {
        name = "symfony-polyfill-php73-cc5db0e22b3cb4111010e48785a97f670b350ca5";
        src = fetchurl {
          url = "https://api.github.com/repos/symfony/polyfill-php73/zipball/cc5db0e22b3cb4111010e48785a97f670b350ca5";
          sha256 = "04z6fah8rn5b01w78j0vqa0jys4mvji66z4ql6wk1r1bf6j0048y";
        };
      };
    };
    "symfony/polyfill-php80" = {
      targetDir = "";
      src = composerEnv.buildZipPackage {
        name = "symfony-polyfill-php80-57b712b08eddb97c762a8caa32c84e037892d2e9";
        src = fetchurl {
          url = "https://api.github.com/repos/symfony/polyfill-php80/zipball/57b712b08eddb97c762a8caa32c84e037892d2e9";
          sha256 = "0dpa53wj3l84f273cnphlps23k09789z8g1znd4h4c7ly6dlqx14";
        };
      };
    };
    "symfony/polyfill-php81" = {
      targetDir = "";
      src = composerEnv.buildZipPackage {
        name = "symfony-polyfill-php81-5de4ba2d41b15f9bd0e19b2ab9674135813ec98f";
        src = fetchurl {
          url = "https://api.github.com/repos/symfony/polyfill-php81/zipball/5de4ba2d41b15f9bd0e19b2ab9674135813ec98f";
          sha256 = "0igxnmib8vkjp9x81j66hl4pf8i0nj86k4hdh8gzcymq01si0mxm";
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
    "symfony/stopwatch" = {
      targetDir = "";
      src = composerEnv.buildZipPackage {
        name = "symfony-stopwatch-16c74a51700ba5a5045f1a67e3095bf08c44ac52";
        src = fetchurl {
          url = "https://api.github.com/repos/symfony/stopwatch/zipball/16c74a51700ba5a5045f1a67e3095bf08c44ac52";
          sha256 = "0wdrynx4jyk833n6mk9m1hfx7bmawr0f9c4kva03qiscrpbpk0ih";
        };
      };
    };
    "symfony/string" = {
      targetDir = "";
      src = composerEnv.buildZipPackage {
        name = "symfony-string-412eda2039ad5466f823c3696c0c309e6c140086";
        src = fetchurl {
          url = "https://api.github.com/repos/symfony/string/zipball/412eda2039ad5466f823c3696c0c309e6c140086";
          sha256 = "124wyxjc73lqwdqv5cjr6gwbl4wlkwnmvrzx9vd312473frpffzi";
        };
      };
    };
    "techlivezheng/phpctags" = {
      targetDir = "";
      src = composerEnv.buildZipPackage {
        name = "techlivezheng-phpctags-3222f56132e71b1f4e2863c799abe4325dbf3a63";
        src = fetchurl {
          url = "https://api.github.com/repos/vim-php/phpctags/zipball/3222f56132e71b1f4e2863c799abe4325dbf3a63";
          sha256 = "14sghif9m2694nzpiwr04hxazg3wc6xcq2zwcinr70w293xa754f";
        };
      };
    };
    "theseer/tokenizer" = {
      targetDir = "";
      src = composerEnv.buildZipPackage {
        name = "theseer-tokenizer-34a41e998c2183e22995f158c581e7b5e755ab9e";
        src = fetchurl {
          url = "https://api.github.com/repos/theseer/tokenizer/zipball/34a41e998c2183e22995f158c581e7b5e755ab9e";
          sha256 = "1za4a017kjb4rw2ydglip4bp5q2y7mfiycj3fvnp145i84jc7n0q";
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
  };
  devPackages = {
    "php-stubs/wordpress-stubs" = {
      targetDir = "";
      src = composerEnv.buildZipPackage {
        name = "php-stubs-wordpress-stubs-794e6eedfd5f2a334d581214c007fc398be588fe";
        src = fetchurl {
          url = "https://api.github.com/repos/php-stubs/wordpress-stubs/zipball/794e6eedfd5f2a334d581214c007fc398be588fe";
          sha256 = "0r39can03ymq7yq0hb0j3dsmxgly1h5x362ffkxdad53znk5qdj1";
        };
      };
    };
    "phpstan/phpstan" = {
      targetDir = "";
      src = composerEnv.buildZipPackage {
        name = "phpstan-phpstan-b4d40f1d759942f523be267a1bab6884f46ca3f7";
        src = fetchurl {
          url = "https://api.github.com/repos/phpstan/phpstan/zipball/b4d40f1d759942f523be267a1bab6884f46ca3f7";
          sha256 = "0sbvbfcjyx6j4yaajy6iqsarf9iqa6pmz34p439kr8j5jcykadm2";
        };
      };
    };
    "szepeviktor/phpstan-wordpress" = {
      targetDir = "";
      src = composerEnv.buildZipPackage {
        name = "szepeviktor-phpstan-wordpress-bdbea69b2ba4a69998c3b6fe2b7106d78a23bd72";
        src = fetchurl {
          url = "https://api.github.com/repos/szepeviktor/phpstan-wordpress/zipball/bdbea69b2ba4a69998c3b6fe2b7106d78a23bd72";
          sha256 = "0501yaarjyfnblpg5c2skgrrgr0d0alrwq35jphjzpiz6f17nrci";
        };
      };
    };
  };
in
composerEnv.buildPackage {
  inherit packages devPackages noDev;
  name = "montchr-dotfield-common";
  src = ./.;
  executable = false;
  symlinkDependencies = false;
  meta = {
    license = "GPL-3.0-or-later";
  };
}
