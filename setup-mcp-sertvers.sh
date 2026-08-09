#!/usr/bin/env bash

CLAUDE() { PATH=/run/current-system/sw/bin:$PATH command claude "$@"; }

cd ~/work/other/kayzen/apps/console
echo "=> $(pwd)"
CLAUDE mcp add --transport http atlassian https://mcp.atlassian.com/v1/mcp
CLAUDE mcp add --transport http figma https://mcp.figma.com/mcp
CLAUDE mcp add perplexity /home/osv/work/kayzen/repos/github.com/Kayzen/ai-utils/mcp/perplexity-advanced-mcp
CLAUDE mcp add mysql-f0 /home/osv/work/kayzen/repos/github.com/Kayzen/ai-utils/mcp/mysql-f0
CLAUDE mcp add mysql-local /home/osv/work/kayzen/repos/github.com/Kayzen/ai-utils/mcp/mysql-local
CLAUDE mcp add mysql-prod /home/osv/work/kayzen/repos/github.com/Kayzen/ai-utils/mcp/mysql-prod
CLAUDE mcp add clickhouse /home/osv/work/kayzen/repos/github.com/Kayzen/ai-utils/mcp/clickhouse
CLAUDE mcp add honeybadger /home/osv/work/kayzen/repos/github.com/Kayzen/ai-utils/mcp/honeybadger

cd ~/work/my/nixos-config
echo "=> $(pwd)"
CLAUDE mcp add perplexity /home/osv/work/kayzen/repos/github.com/Kayzen/ai-utils/mcp/perplexity-advanced-mcp
CLAUDE mcp add nixos uvx mcp-nixos
