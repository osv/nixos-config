{ channels, ... }:

final: prev: {
  clickhouse-cli = channels.unstable.python311Packages.clickhouse-cli.override {
    sqlparse = channels.unstable.python311Packages.sqlparse.overridePythonAttrs (oldAttrs: {
      src = prev.fetchPypi {
        pname = "sqlparse";
        version = "0.4.3";
        hash = "sha256-acqASEa7EU0uw4DkNgqKNA24PwzPOvzusUBN8Cj1cmg=";
      };
      nativeBuildInputs = [
        prev.python311Packages.flit-core
        prev.python311Packages.setuptools
        prev.installShellFiles
      ];
    });
  };
}
