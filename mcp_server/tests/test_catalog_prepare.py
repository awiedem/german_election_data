from __future__ import annotations

import json
from pathlib import Path

import duckdb
import pytest

from gerda_mcp.catalog import CatalogError, load_catalog
from gerda_mcp.prepare import prepare_dataset
from gerda_mcp.store import GerdaStore, StoreError


def test_catalog_rejects_missing_dataset_fields(tmp_path: Path) -> None:
    path = tmp_path / "catalog.json"
    path.write_text(
        json.dumps({"catalog_version": 1, "datasets": {"broken": {}}}),
        encoding="utf-8",
    )
    with pytest.raises(CatalogError, match="is missing"):
        load_catalog(path)


def test_prepare_preserves_identifier_strings(prepared_store) -> None:
    _, _, data_dir = prepared_store
    connection = duckdb.connect()
    try:
        row = connection.execute(
            "SELECT ags, state, county FROM read_parquet(?) ORDER BY election_year LIMIT 1",
            [str(data_dir / "test.parquet")],
        ).fetchone()
        types = {
            item[0]: item[1]
            for item in connection.execute(
                "DESCRIBE SELECT * FROM read_parquet(?)",
                [str(data_dir / "test.parquet")],
            ).fetchall()
        }
    finally:
        connection.close()
    assert row == ("01001000", "01", "01001")
    assert types["ags"] == "VARCHAR"
    assert types["state"] == "VARCHAR"
    assert types["county"] == "VARCHAR"


def test_prepare_rejects_checksum_mismatch(prepared_store, tmp_path: Path) -> None:
    _, catalog_path, _ = prepared_store
    catalog = json.loads(catalog_path.read_text(encoding="utf-8"))
    catalog["datasets"]["federal_muni_harm_25"]["source"]["sha256"] = "0" * 64
    broken_catalog = tmp_path / "broken-catalog.json"
    broken_catalog.write_text(json.dumps(catalog), encoding="utf-8")
    source = tmp_path / "source.csv"
    source.write_text("ags,election_year\n01001000,1990\n", encoding="utf-8")
    catalog["datasets"]["federal_muni_harm_25"]["source"]["size_bytes"] = (
        source.stat().st_size
    )
    broken_catalog.write_text(json.dumps(catalog), encoding="utf-8")
    with pytest.raises(ValueError, match="SHA-256 mismatch"):
        prepare_dataset(
            broken_catalog,
            tmp_path / "broken-data",
            "federal_muni_harm_25",
            source,
        )


def test_store_rejects_altered_artifact(prepared_store) -> None:
    _, catalog_path, data_dir = prepared_store
    artifact = data_dir / "test.parquet"
    artifact.write_bytes(artifact.read_bytes() + b"altered")
    with pytest.raises(StoreError, match="Artifact SHA-256 mismatch"):
        GerdaStore(catalog_path, data_dir)


def test_store_rejects_missing_artifact(prepared_store) -> None:
    _, catalog_path, data_dir = prepared_store
    (data_dir / "test.parquet").unlink()
    with pytest.raises(StoreError, match="Prepared artifact not found"):
        GerdaStore(catalog_path, data_dir)
