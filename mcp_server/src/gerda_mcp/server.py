from __future__ import annotations

import argparse
import logging
import os
import time
from collections.abc import Callable
from pathlib import Path
from typing import Any

from mcp.server.mcpserver import MCPServer
from mcp.types import ToolAnnotations
from starlette.requests import Request
from starlette.responses import JSONResponse

from .store import GerdaStore

LOGGER = logging.getLogger("gerda_mcp")
READ_ONLY = ToolAnnotations(
    readOnlyHint=True,
    destructiveHint=False,
    idempotentHint=True,
    openWorldHint=False,
)


def _paths() -> tuple[Path, Path]:
    project_root = Path(__file__).resolve().parents[2]
    catalog_path = Path(
        os.environ.get("GERDA_MCP_CATALOG", str(project_root / "catalog.json"))
    )
    data_dir = Path(os.environ.get("GERDA_MCP_DATA_DIR", str(project_root / "data")))
    return catalog_path, data_dir


def _logged(tool_name: str, operation: Callable[[], dict[str, Any]]) -> dict[str, Any]:
    started = time.monotonic()
    try:
        result = operation()
    except Exception:
        LOGGER.warning(
            "tool=%s status=error duration_ms=%d",
            tool_name,
            int((time.monotonic() - started) * 1000),
        )
        raise
    row_count = result.get("returned_rows", len(result.get("matches", [])))
    LOGGER.info(
        "tool=%s status=ok duration_ms=%d rows=%d",
        tool_name,
        int((time.monotonic() - started) * 1000),
        row_count,
    )
    return result


def build_server(store: GerdaStore) -> MCPServer:
    server = MCPServer(
        name="gerda",
        title="GERDA German Election Database",
        description="Read-only access to published GERDA election data.",
        instructions=(
            "Use find_geographies before querying names. Treat AGS, state, and county "
            "codes as strings. Party shares and turnout are proportions from 0 to 1. "
            "Always retain the release identifier, harmonization year, caveats, and citation."
        ),
        version="0.1.0",
    )

    @server.tool(annotations=READ_ONLY, structured_output=True)
    def list_datasets() -> dict[str, Any]:
        """List GERDA datasets available through this server."""
        return _logged("list_datasets", store.list_datasets)

    @server.tool(annotations=READ_ONLY, structured_output=True)
    def describe_dataset(
        dataset: str, column_search: str | None = None
    ) -> dict[str, Any]:
        """Describe coverage, caveats, citation, and columns for a GERDA dataset."""
        return _logged(
            "describe_dataset", lambda: store.describe_dataset(dataset, column_search)
        )

    @server.tool(annotations=READ_ONLY, structured_output=True)
    def find_geographies(
        query: str, state: str | None = None, limit: int = 10
    ) -> dict[str, Any]:
        """Resolve a municipality name or AGS prefix to GERDA geographic identifiers."""
        return _logged(
            "find_geographies", lambda: store.find_geographies(query, state, limit)
        )

    @server.tool(annotations=READ_ONLY, structured_output=True)
    def query_results(
        dataset: str,
        metrics: list[str],
        ags: list[str] | None = None,
        years: list[int] | None = None,
        states: list[str] | None = None,
        counties: list[str] | None = None,
        limit: int = 100,
        offset: int = 0,
    ) -> dict[str, Any]:
        """Return a bounded, reproducible subset of a published GERDA dataset."""
        return _logged(
            "query_results",
            lambda: store.query_results(
                dataset,
                metrics,
                ags=ags,
                years=years,
                states=states,
                counties=counties,
                limit=limit,
                offset=offset,
            ),
        )

    @server.tool(annotations=READ_ONLY, structured_output=True)
    def get_download(dataset: str, format: str = "csv") -> dict[str, Any]:
        """Return the full-file URL, checksum, citation, and usage notes."""
        return _logged("get_download", lambda: store.get_download(dataset, format))

    @server.custom_route("/healthz", methods=["GET"])
    async def healthz(_: Request) -> JSONResponse:
        return JSONResponse(
            {
                "status": "ok",
                "service": "gerda-mcp",
                "datasets": len(store.datasets),
            }
        )

    return server


def create_app():
    catalog_path, data_dir = _paths()
    store = GerdaStore(catalog_path, data_dir)
    server = build_server(store)
    return server.streamable_http_app(
        streamable_http_path="/mcp",
        stateless_http=True,
        json_response=True,
        max_request_body_size=1024 * 1024,
        host="127.0.0.1",
    )


def build_parser() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser(description="Run the GERDA MCP server")
    parser.add_argument("--host", default="127.0.0.1")
    parser.add_argument("--port", type=int, default=8000)
    return parser


def main() -> None:
    arguments = build_parser().parse_args()
    logging.basicConfig(
        level=logging.INFO,
        format="%(asctime)s level=%(levelname)s %(name)s %(message)s",
    )
    catalog_path, data_dir = _paths()
    store = GerdaStore(catalog_path, data_dir)
    server = build_server(store)
    server.run(
        transport="streamable-http",
        host=arguments.host,
        port=arguments.port,
        streamable_http_path="/mcp",
        stateless_http=True,
        json_response=True,
        max_request_body_size=1024 * 1024,
    )


if __name__ == "__main__":
    main()
