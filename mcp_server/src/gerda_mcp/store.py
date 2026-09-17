from __future__ import annotations

import datetime as dt
import hashlib
import json
import math
from collections.abc import Iterable
from pathlib import Path
from typing import Any

import duckdb

from .catalog import load_catalog


class StoreError(ValueError):
    """Raised for invalid artifacts or tool inputs."""


def _sha256_file(path: Path) -> str:
    digest = hashlib.sha256()
    with path.open("rb") as handle:
        for chunk in iter(lambda: handle.read(1024 * 1024), b""):
            digest.update(chunk)
    return digest.hexdigest()


def _json_value(value: Any) -> Any:
    if value is None:
        return None
    if isinstance(value, (dt.date, dt.datetime)):
        return value.isoformat()
    if isinstance(value, float) and (math.isnan(value) or math.isinf(value)):
        return None
    return value


def _quote_identifier(name: str) -> str:
    return '"' + name.replace('"', '""') + '"'


def _deduplicate(values: Iterable[str]) -> list[str]:
    return list(dict.fromkeys(values))


class GerdaStore:
    def __init__(self, catalog_path: Path, data_dir: Path):
        self.catalog_path = catalog_path
        self.data_dir = data_dir
        self.catalog = load_catalog(catalog_path)
        self.datasets: dict[str, dict[str, Any]] = self.catalog["datasets"]
        self._schemas: dict[str, list[dict[str, str]]] = {}
        self._column_groups: dict[str, dict[str, str]] = {}
        self._validate_artifacts()

    def _dataset(self, name: str) -> dict[str, Any]:
        try:
            return self.datasets[name]
        except KeyError as exc:
            available = ", ".join(sorted(self.datasets))
            raise StoreError(
                f"Unknown dataset {name!r}; available: {available}"
            ) from exc

    def _artifact_path(self, dataset: dict[str, Any]) -> Path:
        return self.data_dir / dataset["artifact"]["filename"]

    def _metadata_path(self, dataset: dict[str, Any]) -> Path:
        return self.data_dir / dataset["artifact"]["metadata_filename"]

    def _validate_artifacts(self) -> None:
        for name, dataset in self.datasets.items():
            artifact_path = self._artifact_path(dataset)
            metadata_path = self._metadata_path(dataset)
            if not artifact_path.is_file():
                raise StoreError(
                    f"Prepared artifact not found for {name}: {artifact_path}"
                )
            if not metadata_path.is_file():
                raise StoreError(
                    f"Artifact metadata not found for {name}: {metadata_path}"
                )

            try:
                metadata = json.loads(metadata_path.read_text(encoding="utf-8"))
            except (OSError, json.JSONDecodeError) as exc:
                raise StoreError(
                    f"Invalid artifact metadata for {name}: {exc}"
                ) from exc

            if metadata.get("dataset") != name:
                raise StoreError(f"Artifact metadata dataset mismatch for {name}")
            if metadata.get("release_identifier") != dataset["release_identifier"]:
                raise StoreError(f"Artifact release mismatch for {name}")
            if metadata.get("source_sha256") != dataset["source"]["sha256"]:
                raise StoreError(f"Artifact source checksum mismatch for {name}")
            if metadata.get("row_count") != dataset["artifact"]["expected_rows"]:
                raise StoreError(f"Artifact row-count metadata mismatch for {name}")

            actual_artifact_sha = _sha256_file(artifact_path)
            if actual_artifact_sha != metadata.get("artifact_sha256"):
                raise StoreError(f"Artifact SHA-256 mismatch for {name}")

            connection = duckdb.connect()
            try:
                description = connection.execute(
                    "DESCRIBE SELECT * FROM read_parquet(?)", [str(artifact_path)]
                ).fetchall()
                row_count = connection.execute(
                    "SELECT count(*) FROM read_parquet(?)", [str(artifact_path)]
                ).fetchone()[0]
            except duckdb.Error as exc:
                raise StoreError(
                    f"Cannot read prepared artifact for {name}: {exc}"
                ) from exc
            finally:
                connection.close()

            if row_count != dataset["artifact"]["expected_rows"]:
                raise StoreError(f"Artifact row-count mismatch for {name}")

            schema = [{"name": row[0], "type": row[1]} for row in description]
            columns = [column["name"] for column in schema]
            if columns != metadata.get("columns"):
                raise StoreError(f"Artifact schema does not match metadata for {name}")
            missing = sorted(set(dataset["required_columns"]) - set(columns))
            if missing:
                raise StoreError(f"Artifact {name} is missing: {', '.join(missing)}")
            for identifier in ("ags", "state", "county"):
                kind = next(
                    column["type"] for column in schema if column["name"] == identifier
                )
                if kind.upper() not in {"VARCHAR", "TEXT"}:
                    raise StoreError(
                        f"Artifact {name} column {identifier} is not a string"
                    )

            self._schemas[name] = schema
            self._column_groups[name] = self._expand_groups(dataset, columns)

    @staticmethod
    def _expand_groups(dataset: dict[str, Any], columns: list[str]) -> dict[str, str]:
        groups: dict[str, str] = {}
        for group_name, definition in dataset["schema_groups"].items():
            if "columns" in definition:
                group_columns = definition["columns"]
            else:
                first = definition["first_column"]
                last = definition["last_column"]
                try:
                    first_index = columns.index(first)
                    last_index = columns.index(last)
                except ValueError as exc:
                    raise StoreError(
                        f"Schema group {group_name!r} has an unknown boundary column"
                    ) from exc
                if last_index < first_index:
                    raise StoreError(
                        f"Schema group {group_name!r} has reversed boundaries"
                    )
                group_columns = columns[first_index : last_index + 1]
            missing = sorted(set(group_columns) - set(columns))
            if missing:
                raise StoreError(
                    f"Schema group {group_name!r} has unknown columns: {', '.join(missing)}"
                )
            for column in group_columns:
                if column in groups:
                    raise StoreError(
                        f"Column {column!r} belongs to multiple schema groups"
                    )
                groups[column] = group_name
        return groups

    def list_datasets(self) -> dict[str, Any]:
        items = []
        for name, dataset in sorted(self.datasets.items()):
            items.append(
                {
                    "name": name,
                    "title": dataset["title"],
                    "geographic_level": dataset["geographic_level"],
                    "coverage": dataset["coverage"],
                    "harmonization_year": dataset["harmonization_year"],
                    "release_identifier": dataset["release_identifier"],
                    "license_status": dataset["license"]["status"],
                    "public_launch_approved": dataset["license"][
                        "public_launch_approved"
                    ],
                }
            )
        return {"catalog_version": self.catalog["catalog_version"], "datasets": items}

    def describe_dataset(
        self, dataset_name: str, column_search: str | None = None
    ) -> dict[str, Any]:
        dataset = self._dataset(dataset_name)
        schema = self._schemas[dataset_name]
        groups = self._column_groups[dataset_name]
        definitions = dataset.get("column_definitions", {})
        search = (column_search or "").strip().casefold()

        columns = []
        for column in schema:
            name = column["name"]
            group = groups.get(name, "other")
            description = definitions.get(name)
            if description is None and group == "party_shares":
                description = f"Vote share for {name}, or a GERDA party aggregate; proportion from 0 to 1."
            elif description is None:
                description = f"GERDA {group.replace('_', ' ')} field."
            if (
                search
                and search not in name.casefold()
                and search not in description.casefold()
            ):
                continue
            columns.append(
                {
                    "name": name,
                    "type": column["type"],
                    "group": group,
                    "description": description,
                }
            )
            if len(columns) == 50:
                break

        matching_total = sum(
            1
            for column in schema
            if not search
            or search in column["name"].casefold()
            or search
            in definitions.get(
                column["name"],
                (
                    f"Vote share for {column['name']}, or a GERDA party aggregate; "
                    "proportion from 0 to 1."
                    if groups.get(column["name"]) == "party_shares"
                    else f"GERDA {groups.get(column['name'], 'other').replace('_', ' ')} field."
                ),
            ).casefold()
        )

        group_summary: dict[str, int] = {}
        for group in groups.values():
            group_summary[group] = group_summary.get(group, 0) + 1

        return {
            "name": dataset_name,
            "title": dataset["title"],
            "description": dataset["description"],
            "geographic_level": dataset["geographic_level"],
            "coverage": dataset["coverage"],
            "harmonization_year": dataset["harmonization_year"],
            "release_identifier": dataset["release_identifier"],
            "row_count": dataset["artifact"]["expected_rows"],
            "column_count": len(schema),
            "schema_groups": group_summary,
            "columns": columns,
            "matching_column_count": matching_total,
            "columns_truncated": matching_total > len(columns),
            "usage_notes": dataset["usage_notes"],
            "caveats": dataset["caveats"],
            "citation": dataset["citation"],
            "license": dataset["license"],
        }

    def find_geographies(
        self, query: str, state: str | None = None, limit: int = 10
    ) -> dict[str, Any]:
        dataset_name = "federal_muni_harm_25"
        dataset = self._dataset(dataset_name)
        query = query.strip()
        if not query:
            raise StoreError("query must not be empty")
        if not 1 <= limit <= 25:
            raise StoreError("limit must be between 1 and 25")
        state = state.strip() if state else None
        if state and not (len(state) == 2 and state.isdigit()) and len(state) > 64:
            raise StoreError("state must be a two-digit code or a state name")

        conditions = ["(ags LIKE ? OR ags_name ILIKE ?)"]
        parameters: list[Any] = [f"{query}%", f"%{query}%"]
        if state:
            conditions.append("(state = ? OR state_name ILIKE ?)")
            parameters.extend([state, state])

        sql = f"""
            SELECT DISTINCT ags, ags_name, state, state_name, county
            FROM read_parquet(?)
            WHERE {" AND ".join(conditions)}
            ORDER BY
              CASE WHEN ags = ? THEN 0 WHEN ags_name ILIKE ? THEN 1
                   WHEN ags_name ILIKE ? THEN 2 ELSE 3 END,
              ags_name,
              ags
            LIMIT ?
        """
        all_parameters = [
            str(self._artifact_path(dataset)),
            *parameters,
            query,
            query,
            f"{query}%",
            limit,
        ]
        connection = duckdb.connect()
        try:
            rows = connection.execute(sql, all_parameters).fetchall()
        finally:
            connection.close()
        keys = ["ags", "ags_name", "state", "state_name", "county"]
        return {
            "query": query,
            "state": state,
            "matches": [dict(zip(keys, row, strict=True)) for row in rows],
            "limit": limit,
            "release_identifier": dataset["release_identifier"],
        }

    def query_results(
        self,
        dataset_name: str,
        metrics: list[str],
        ags: list[str] | None = None,
        years: list[int] | None = None,
        states: list[str] | None = None,
        counties: list[str] | None = None,
        limit: int = 100,
        offset: int = 0,
    ) -> dict[str, Any]:
        dataset = self._dataset(dataset_name)
        schema_names = {column["name"] for column in self._schemas[dataset_name]}
        metrics = _deduplicate(metric.strip() for metric in metrics if metric.strip())
        if not metrics:
            raise StoreError("metrics must contain at least one column")
        if len(metrics) > 20:
            raise StoreError("metrics may contain at most 20 columns")
        unknown_metrics = sorted(set(metrics) - schema_names)
        if unknown_metrics:
            raise StoreError(f"Unknown metrics: {', '.join(unknown_metrics)}")
        if not 1 <= limit <= 200:
            raise StoreError("limit must be between 1 and 200")
        if not 0 <= offset <= 100000:
            raise StoreError("offset must be between 0 and 100000")

        ags = _deduplicate(ags or [])
        years = list(dict.fromkeys(years or []))
        states = _deduplicate(states or [])
        counties = _deduplicate(counties or [])
        if len(ags) > 50:
            raise StoreError("ags may contain at most 50 identifiers")
        if len(years) > 20:
            raise StoreError("years may contain at most 20 values")
        if len(states) > 16:
            raise StoreError("states may contain at most 16 values")
        if len(counties) > 50:
            raise StoreError("counties may contain at most 50 identifiers")
        invalid_ags = [value for value in ags if len(value) != 8 or not value.isdigit()]
        if invalid_ags:
            raise StoreError("Each AGS must contain exactly eight digits")
        invalid_counties = [
            value for value in counties if len(value) != 5 or not value.isdigit()
        ]
        if invalid_counties:
            raise StoreError("Each county identifier must contain exactly five digits")
        invalid_states = [
            value
            for value in states
            if not (len(value) == 2 and value.isdigit()) and len(value) > 64
        ]
        if invalid_states:
            raise StoreError("Each state must be a two-digit code or a state name")
        coverage = dataset["coverage"]
        invalid_years = [
            year
            for year in years
            if not isinstance(year, int)
            or isinstance(year, bool)
            or not coverage["start_year"] <= year <= coverage["end_year"]
        ]
        if invalid_years:
            raise StoreError(
                f"years must fall between {coverage['start_year']} and {coverage['end_year']}"
            )

        conditions: list[str] = []
        parameters: list[Any] = []
        if ags:
            conditions.append(f"ags IN ({', '.join('?' for _ in ags)})")
            parameters.extend(ags)
        if years:
            conditions.append(f"election_year IN ({', '.join('?' for _ in years)})")
            parameters.extend(years)
        if counties:
            conditions.append(f"county IN ({', '.join('?' for _ in counties)})")
            parameters.extend(counties)
        if states:
            state_parts = []
            for state in states:
                state_parts.append("(state = ? OR state_name ILIKE ?)")
                parameters.extend([state, state])
            conditions.append(f"({' OR '.join(state_parts)})")

        where = f"WHERE {' AND '.join(conditions)}" if conditions else ""
        selected = _deduplicate([*dataset["base_columns"], *metrics])
        selected_sql = ", ".join(_quote_identifier(column) for column in selected)
        artifact_path = str(self._artifact_path(dataset))
        query_sql = f"""
            SELECT {selected_sql}
            FROM read_parquet(?)
            {where}
            ORDER BY ags, election_year
            LIMIT ? OFFSET ?
        """
        count_sql = f"SELECT count(*) FROM read_parquet(?) {where}"

        connection = duckdb.connect()
        try:
            rows = connection.execute(
                query_sql, [artifact_path, *parameters, limit, offset]
            ).fetchall()
            total_matches = connection.execute(
                count_sql, [artifact_path, *parameters]
            ).fetchone()[0]
        finally:
            connection.close()

        records = [
            {key: _json_value(value) for key, value in zip(selected, row, strict=True)}
            for row in rows
        ]
        return {
            "dataset": dataset_name,
            "release_identifier": dataset["release_identifier"],
            "harmonization_year": dataset["harmonization_year"],
            "applied_filters": {
                "ags": ags,
                "years": years,
                "states": states,
                "counties": counties,
                "metrics": metrics,
            },
            "offset": offset,
            "limit": limit,
            "returned_rows": len(records),
            "total_matches": total_matches,
            "truncated": offset + len(records) < total_matches,
            "rows": records,
            "caveats": dataset["caveats"],
        }

    def get_download(
        self, dataset_name: str, file_format: str = "csv"
    ) -> dict[str, Any]:
        dataset = self._dataset(dataset_name)
        if file_format.casefold() != "csv":
            raise StoreError("Only csv is available in the pilot")
        return {
            "dataset": dataset_name,
            "format": "csv",
            "url": dataset["source"]["canonical_download_url"],
            "pinned_source_url": dataset["source"]["url"],
            "size_bytes": dataset["source"]["size_bytes"],
            "sha256": dataset["source"]["sha256"],
            "release_identifier": dataset["release_identifier"],
            "citation": dataset["citation"],
            "usage_notes": dataset["usage_notes"],
            "license": dataset["license"],
        }
