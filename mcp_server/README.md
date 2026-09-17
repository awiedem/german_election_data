# GERDA MCP pilot

This directory contains a read-only Model Context Protocol (MCP) server for the
GERDA dataset `federal_muni_harm_25`. It supports dataset discovery,
municipality lookup, bounded result queries, and citation-aware downloads.

## Status

The public endpoint is `https://mcp.german-elections.com/mcp`. This is an
experimental service with no availability guarantee. The federal-data reuse
terms are published in [`DATA_REUSE_STATEMENT.md`](DATA_REUSE_STATEMENT.md), and
the catalog exposes the license status to clients.

## Tools

- `list_datasets`: list available datasets and release information.
- `describe_dataset`: inspect coverage, caveats, citation, and columns.
- `find_geographies`: resolve municipality names and AGS prefixes.
- `query_results`: retrieve at most 200 rows and 20 requested metrics.
- `get_download`: obtain the complete CSV URL, checksum, and citation.

All tools are read-only. The server does not accept SQL, filesystem paths, or
arbitrary URLs. AGS, state, and county identifiers remain strings. Party shares
and turnout remain proportions from 0 to 1.

## Local setup

From this directory:

```sh
UV_CACHE_DIR=/tmp/gerda-uv-cache uv sync --frozen
uv run gerda-mcp-prepare \
  --data-dir ./data \
  --source-file ../data/federal_elections/municipality_level/final/federal_muni_harm_25.csv
uv run gerda-mcp --host 127.0.0.1 --port 8000
```

The MCP endpoint is `http://127.0.0.1:8000/mcp`; the health endpoint is
`http://127.0.0.1:8000/healthz`.

For local Codex testing, add the running HTTP server:

```sh
codex mcp add gerda --url http://127.0.0.1:8000/mcp
```

Remove or disable that local configuration before adding the public URL.

For the public service, use:

```sh
codex mcp add gerda --url https://mcp.german-elections.com/mcp
```

## Tests

```sh
UV_CACHE_DIR=/tmp/gerda-uv-cache uv run --frozen pytest
```

The test suite uses a small generated fixture. A release check should also run
the preparation command against the full pinned CSV and verify a known
Flensburg 1990 query:

```sh
GERDA_MCP_RELEASE_DATA_DIR=./data \
GERDA_MCP_RELEASE_SOURCE=../data/federal_elections/municipality_level/final/federal_muni_harm_25.csv \
UV_CACHE_DIR=/tmp/gerda-uv-cache uv run --frozen pytest tests/test_release_data.py
```

## Citation

Heddesheimer, Vincent, Hanno Hilbig, Florian Sichart, and Andreas Wiedemann.
2025. “GERDA: The German Election Database.” *Scientific Data* 12: 618.
<https://doi.org/10.1038/s41597-025-04811-5>

## Deployment

The `deploy/` directory contains the systemd, Nginx, rate-limit, and log-rotation
definitions. The application must run as the non-login `gerda-mcp` user and
bind only to `127.0.0.1:8000`. Nginx terminates TLS and limits MCP requests to 60
per minute per IP address with a burst of 10.
