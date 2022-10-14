{self, ...}: {
  perPackage = ctx @ {...}: {
    apps.archivebox = {
      type = "app";
      program = ctx.config.packages.archivebox;
    };
  };
}
