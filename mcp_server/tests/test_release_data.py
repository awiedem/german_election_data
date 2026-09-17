from __future__ import annotations

import csv
import os
from pathlib import Path

import pytest

from gerda_mcp.store import GerdaStore


def test_full_release_flensburg_matches_source() -> None:
    data_dir_value = os.environ.get("GERDA_MCP_RELEASE_DATA_DIR")
    source_value = os.environ.get("GERDA_MCP_RELEASE_SOURCE")
    if not data_dir_value or not source_value:
        pytest.skip("full release paths were not supplied")

    data_dir = Path(data_dir_value)
    source_path = Path(source_value)
    catalog_path = Path(__file__).resolve().parents[1] / "catalog.json"
    store = GerdaStore(catalog_path, data_dir)
    result = store.query_results(
        "federal_muni_harm_25",
        ["turnout", "spd", "afd"],
        ags=["01001000"],
        years=[1990],
        limit=10,
    )
    assert result["total_matches"] == 1
    row = result["rows"][0]

    with source_path.open(newline="", encoding="utf-8") as handle:
        source = next(
            item
            for item in csv.DictReader(handle)
            if item["ags"] == "01001000" and item["election_year"] == "1990"
        )

    assert row["ags"] == "01001000"
    assert row["turnout"] == float(source["turnout"])
    assert row["spd"] == float(source["spd"])
    assert row["afd"] is None
    assert source["afd"] == ""
