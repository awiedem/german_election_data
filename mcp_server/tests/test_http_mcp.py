from __future__ import annotations

import asyncio
import socket
import threading
import time
import urllib.request

import uvicorn
from mcp.client import Client

from gerda_mcp.server import build_server


def _free_port() -> int:
    with socket.socket() as sock:
        sock.bind(("127.0.0.1", 0))
        return sock.getsockname()[1]


def test_all_tools_over_streamable_http(prepared_store) -> None:
    store, _, _ = prepared_store
    mcp_server = build_server(store)
    app = mcp_server.streamable_http_app(
        streamable_http_path="/mcp",
        stateless_http=True,
        json_response=True,
        max_request_body_size=1024 * 1024,
    )
    port = _free_port()
    server = uvicorn.Server(
        uvicorn.Config(app, host="127.0.0.1", port=port, log_level="error")
    )
    thread = threading.Thread(target=server.run, daemon=True)
    thread.start()
    try:
        for _ in range(100):
            try:
                with urllib.request.urlopen(
                    f"http://127.0.0.1:{port}/healthz", timeout=0.2
                ) as response:
                    if response.status == 200:
                        break
            except OSError:
                time.sleep(0.02)
        else:
            raise AssertionError("Test server did not start")

        async def exercise() -> None:
            async with Client(f"http://127.0.0.1:{port}/mcp") as client:
                tools = await client.list_tools()
                assert {tool.name for tool in tools.tools} == {
                    "list_datasets",
                    "describe_dataset",
                    "find_geographies",
                    "query_results",
                    "get_download",
                }
                calls = [
                    ("list_datasets", {}),
                    ("describe_dataset", {"dataset": "federal_muni_harm_25"}),
                    ("find_geographies", {"query": "Flensburg"}),
                    (
                        "query_results",
                        {
                            "dataset": "federal_muni_harm_25",
                            "metrics": ["afd"],
                            "ags": ["01001000"],
                        },
                    ),
                    ("get_download", {"dataset": "federal_muni_harm_25"}),
                ]
                for name, arguments in calls:
                    result = await client.call_tool(name, arguments)
                    assert result.is_error is not True
                    assert result.structured_content is not None

                invalid = await client.call_tool(
                    "query_results",
                    {"dataset": "federal_muni_harm_25", "metrics": ["unknown"]},
                )
                assert invalid.is_error is True

        asyncio.run(exercise())
    finally:
        server.should_exit = True
        thread.join(timeout=5)
        assert not thread.is_alive()
