from __future__ import annotations

import json
from pathlib import Path
from typing import Any


class CatalogError(ValueError):
    """Raised when the dataset catalog is missing or invalid."""


REQUIRED_DATASET_KEYS = {
    "title",
    "description",
    "geographic_level",
    "coverage",
    "harmonization_year",
    "release_identifier",
    "source",
    "artifact",
    "license",
    "citation",
    "usage_notes",
    "caveats",
    "required_columns",
    "base_columns",
    "schema_groups",
}


def load_catalog(path: Path) -> dict[str, Any]:
    if not path.is_file():
        raise CatalogError(f"Catalog file not found: {path}")

    try:
        catalog = json.loads(path.read_text(encoding="utf-8"))
    except (OSError, json.JSONDecodeError) as exc:
        raise CatalogError(f"Cannot read catalog: {exc}") from exc

    if catalog.get("catalog_version") != 1:
        raise CatalogError("Unsupported or missing catalog_version; expected 1")
    datasets = catalog.get("datasets")
    if not isinstance(datasets, dict) or not datasets:
        raise CatalogError("Catalog must contain at least one dataset")

    for name, dataset in datasets.items():
        if not isinstance(name, str) or not name:
            raise CatalogError("Dataset names must be non-empty strings")
        if not isinstance(dataset, dict):
            raise CatalogError(f"Dataset {name!r} must be an object")
        missing = sorted(REQUIRED_DATASET_KEYS - dataset.keys())
        if missing:
            raise CatalogError(f"Dataset {name!r} is missing: {', '.join(missing)}")

        source = dataset["source"]
        artifact = dataset["artifact"]
        if len(source.get("sha256", "")) != 64:
            raise CatalogError(f"Dataset {name!r} has an invalid source SHA-256")
        if (
            not isinstance(artifact.get("expected_rows"), int)
            or artifact["expected_rows"] <= 0
        ):
            raise CatalogError(f"Dataset {name!r} has an invalid expected row count")
        if not dataset["base_columns"] or not dataset["required_columns"]:
            raise CatalogError(
                f"Dataset {name!r} must declare base and required columns"
            )

    return catalog
