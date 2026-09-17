from __future__ import annotations

import pytest

from gerda_mcp.store import StoreError


def test_list_and_describe_dataset(prepared_store) -> None:
    store, _, _ = prepared_store
    listed = store.list_datasets()
    assert listed["datasets"][0]["name"] == "federal_muni_harm_25"
    description = store.describe_dataset("federal_muni_harm_25", "afd")
    assert description["matching_column_count"] == 1
    assert description["columns"][0]["group"] == "party_shares"


def test_find_geographies_by_name_and_ags(prepared_store) -> None:
    store, _, _ = prepared_store
    by_name = store.find_geographies("Flensburg")
    by_ags = store.find_geographies("01001")
    assert by_name["matches"][0]["ags"] == "01001000"
    assert by_ags["matches"][0]["ags_name"] == "Flensburg, Stadt"


def test_find_geographies_state_filter_and_limits(prepared_store) -> None:
    store, _, _ = prepared_store
    assert (
        store.find_geographies("Dresden", state="14")["matches"][0]["ags"] == "14612000"
    )
    assert store.find_geographies("Dresden", state="01")["matches"] == []
    with pytest.raises(StoreError, match="between 1 and 25"):
        store.find_geographies("Dresden", limit=26)


def test_query_filters_order_pagination_and_nulls(prepared_store) -> None:
    store, _, _ = prepared_store
    result = store.query_results(
        "federal_muni_harm_25",
        ["afd", "turnout"],
        ags=["01001000"],
        years=[1990, 2025],
        limit=1,
    )
    assert result["total_matches"] == 2
    assert result["returned_rows"] == 1
    assert result["truncated"] is True
    assert result["rows"][0]["election_year"] == 1990
    assert result["rows"][0]["ags"] == "01001000"
    assert result["rows"][0]["afd"] is None

    second = store.query_results(
        "federal_muni_harm_25", ["afd"], ags=["01001000"], limit=1, offset=1
    )
    assert second["rows"][0]["election_year"] == 2025
    assert second["truncated"] is False

    empty = store.query_results("federal_muni_harm_25", ["afd"], ags=["99999999"])
    assert empty["total_matches"] == 0
    assert empty["rows"] == []


@pytest.mark.parametrize(
    ("kwargs", "message"),
    [
        ({"metrics": ["not_a_column"]}, "Unknown metrics"),
        ({"metrics": ["afd"], "ags": ["1001"]}, "exactly eight digits"),
        ({"metrics": ["afd"], "counties": ["0101"]}, "exactly five digits"),
        ({"metrics": ["afd"], "years": [1989]}, "between 1990 and 2025"),
        ({"metrics": ["afd"], "limit": 201}, "between 1 and 200"),
        ({"metrics": ["afd"], "offset": -1}, "between 0 and 100000"),
        ({"metrics": [f"metric_{index}" for index in range(21)]}, "at most 20"),
    ],
)
def test_query_validation(prepared_store, kwargs, message) -> None:
    store, _, _ = prepared_store
    with pytest.raises(StoreError, match=message):
        store.query_results("federal_muni_harm_25", **kwargs)


def test_download_metadata(prepared_store) -> None:
    store, _, _ = prepared_store
    download = store.get_download("federal_muni_harm_25")
    assert download["format"] == "csv"
    assert download["sha256"]
    with pytest.raises(StoreError, match="Only csv"):
        store.get_download("federal_muni_harm_25", "rds")


def test_unknown_dataset(prepared_store) -> None:
    store, _, _ = prepared_store
    with pytest.raises(StoreError, match="Unknown dataset"):
        store.describe_dataset("unknown")
