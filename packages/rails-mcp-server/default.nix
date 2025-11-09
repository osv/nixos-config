{ lib, bundlerApp, ruby, makeWrapper, ... }:

bundlerApp {
  pname = "rails-mcp-server";
  ruby = ruby;
  gemdir = ./.;
  exes = [ "rails-mcp-server" "rails-mcp-setup-claude" "rails-mcp-server-download-resources" ];

  meta = with lib; {
    description = "A Model Context Protocol server for Rails projects";
    homepage = "https://github.com/maquina-app/rails-mcp-server";
    license = licenses.mit;
    maintainers = [ ];
    platforms = platforms.unix;
  };
}