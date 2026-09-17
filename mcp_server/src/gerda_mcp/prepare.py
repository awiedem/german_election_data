from __future__ import annotations

import argparse
import hashlib
import json
import os
import shutil
import tempfile
import urllib.request
from pathlib import Path

import duckdb

from .catalog import load_catalog

IDENTIFIER_TYPES = {
    "ags": "VARCHAR",
    "state": "VARCHAR",
    "county": "VARCHAR",
}


def sha256_file(path: Path) -> str:
    digest = hashlib.sha256()
    with path.open("rb") as handle:
        for chunk in iter(lambda: handle.read(1024 * 1024), b""):
            digest.update(chunk)
    return digest.hexdigest()


def _download(url: str, destination: Path) -> None:
    request = urllib.request.Request(url, headers={"User-Agent": "GERDA-MCP/0.1"})
    with (
        urllib.request.urlopen(request, timeout=120) as response,
        destination.open("wb") as output,
    ):
        shutil.copyfileobj(response, output)


def prepare_dataset(
    catalog_path: Path,
    data_dir: Path,
    dataset_name: str,
    source_file: Path | None = None,
) -> dict[str, object]:
    catalog = load_catalog(catalog_path)
    try:
        dataset = catalog["datasets"][dataset_name]
    except KeyError as exc:
        raise ValueError(f"Unknown dataset: {dataset_name}") from exc

    data_dir.mkdir(parents=True, exist_ok=True)
    artifact_path = data_dir / dataset["artifact"]["filename"]
    metadata_path = data_dir / dataset["artifact"]["metadata_filename"]

    with tempfile.TemporaryDirectory(prefix="gerda-mcp-", dir=data_dir) as temporary:
        temporary_dir = Path(temporary)
        csv_path = temporary_dir / f"{dataset_name}.csv"
        parquet_path = temporary_dir / f"{dataset_name}.parquet"

        if source_file is None:
            _download(dataset["source"]["url"], csv_path)
        else:
            if not source_file.is_file():
                raise ValueError(f"Source file not found: {source_file}")
            shutil.copyfile(source_file, csv_path)

        actual_size = csv_path.stat().st_size
        expected_size = dataset["source"]["size_bytes"]
        if actual_size != expected_size:
            raise ValueError(
                f"Source size mismatch: expected {expected_size}, found {actual_size}"
            )

        actual_source_sha = sha256_file(csv_path)
        expected_source_sha = dataset["source"]["sha256"]
        if actual_source_sha != expected_source_sha:
            raise ValueError(
                "Source SHA-256 mismatch: "
                f"expected {expected_source_sha}, found {actual_source_sha}"
            )

        connection = duckdb.connect()
        try:
            connection.execute(
                """
                CREATE TABLE prepared AS
                SELECT *
                FROM read_csv(
                    ?,
                    header = true,
                    auto_detect = true,
                    types = {'ags': 'VARCHAR', 'state': 'VARCHAR', 'county': 'VARCHAR'}
                )
                """,
                [str(csv_path)],
            )
            row_count = connection.execute("SELECT count(*) FROM prepared").fetchone()[
                0
            ]
            expected_rows = dataset["artifact"]["expected_rows"]
            if row_count != expected_rows:
                raise ValueError(
                    f"Row-count mismatch: expected {expected_rows}, found {row_count}"
                )

            columns = [
                row[0] for row in connection.execute("DESCRIBE prepared").fetchall()
            ]
            missing = sorted(set(dataset["required_columns"]) - set(columns))
            if missing:
                raise ValueError(
                    f"Prepared data are missing columns: {', '.join(missing)}"
                )

            identifier_types = {
                row[0]: row[1]
                for row in connection.execute("DESCRIBE prepared").fetchall()
                if row[0] in IDENTIFIER_TYPES
            }
            invalid_types = {
                name: kind
                for name, kind in identifier_types.items()
                if kind.upper() not in {"VARCHAR", "TEXT"}
            }
            if invalid_types:
                raise ValueError(f"Identifier columns are not strings: {invalid_types}")

            connection.execute(
                "COPY prepared TO ? (FORMAT PARQUET, COMPRESSION ZSTD)",
                [str(parquet_path)],
            )
        finally:
            connection.close()

        artifact_sha = sha256_file(parquet_path)
        metadata = {
            "dataset": dataset_name,
            "release_identifier": dataset["release_identifier"],
            "source_sha256": actual_source_sha,
            "artifact_sha256": artifact_sha,
            "row_count": row_count,
            "columns": columns,
        }
        metadata_temp = temporary_dir / "metadata.json"
        metadata_temp.write_text(
            json.dumps(metadata, indent=2, sort_keys=True) + "\n", encoding="utf-8"
        )

        os.replace(parquet_path, artifact_path)
        os.replace(metadata_temp, metadata_path)

    return metadata


def build_parser() -> argparse.ArgumentParser:
    package_root = Path(__file__).resolve().parents[2]
    parser = argparse.ArgumentParser(
        description="Prepare GERDA data for the MCP server"
    )
    parser.add_argument("--catalog", type=Path, default=package_root / "catalog.json")
    parser.add_argument("--data-dir", type=Path, required=True)
    parser.add_argument("--dataset", default="federal_muni_harm_25")
    parser.add_argument("--source-file", type=Path)
    return parser


def main() -> None:
    arguments = build_parser().parse_args()
    metadata = prepare_dataset(
        catalog_path=arguments.catalog,
        data_dir=arguments.data_dir,
        dataset_name=arguments.dataset,
        source_file=arguments.source_file,
    )
    print(json.dumps(metadata, indent=2, sort_keys=True))


if __name__ == "__main__":
    main()
