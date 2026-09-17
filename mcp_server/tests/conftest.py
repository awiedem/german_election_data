from __future__ import annotations

import hashlib
import json
from pathlib import Path

import pytest

from gerda_mcp.prepare import prepare_dataset
from gerda_mcp.store import GerdaStore

SMALL_CSV = """ags,election_year,election_date,ags_name,state_name,state,county,turnout,afd,flag_total_votes_incongruent
01001000,1990,1990-12-02,"Flensburg, Stadt",Schleswig-Holstein,01,01001,0.725745008122134,,false
01001000,2025,2025-02-23,"Flensburg, Stadt",Schleswig-Holstein,01,01001,0.8123,0.1523,false
14612000,2025,2025-02-23,Dresden,Sachsen,14,14612,0.821,0.271,true
"""


@pytest.fixture
def prepared_store(tmp_path: Path) -> tuple[GerdaStore, Path, Path]:
    source_path = tmp_path / "source.csv"
    source_path.write_text(SMALL_CSV, encoding="utf-8")
    source_sha = hashlib.sha256(source_path.read_bytes()).hexdigest()

    catalog = {
        "catalog_version": 1,
        "datasets": {
            "federal_muni_harm_25": {
                "title": "Test federal data",
                "description": "Test data.",
                "geographic_level": "municipality",
                "coverage": {"start_year": 1990, "end_year": 2025},
                "harmonization_year": 2025,
                "release_identifier": "test:1",
                "source": {
                    "commit": "test",
                    "url": "https://example.invalid/source.csv",
                    "canonical_download_url": "https://example.invalid/download.csv",
                    "size_bytes": source_path.stat().st_size,
                    "sha256": source_sha,
                },
                "artifact": {
                    "filename": "test.parquet",
                    "metadata_filename": "test.metadata.json",
                    "expected_rows": 3,
                },
                "license": {
                    "status": "test",
                    "public_launch_approved": False,
                    "notice": "Test only.",
                },
                "citation": {"text": "Test citation", "doi": "https://example.invalid"},
                "usage_notes": ["Shares are proportions."],
                "caveats": ["Test caveat."],
                "required_columns": [
                    "ags",
                    "election_year",
                    "election_date",
                    "ags_name",
                    "state_name",
                    "state",
                    "county",
                    "turnout",
                    "afd",
                    "flag_total_votes_incongruent",
                ],
                "base_columns": [
                    "ags",
                    "ags_name",
                    "state",
                    "state_name",
                    "county",
                    "election_year",
                    "election_date",
                ],
                "schema_groups": {
                    "identifiers": {
                        "columns": [
                            "ags",
                            "election_year",
                            "election_date",
                            "ags_name",
                            "state_name",
                            "state",
                            "county",
                        ]
                    },
                    "turnout": {"columns": ["turnout"]},
                    "party_shares": {"first_column": "afd", "last_column": "afd"},
                    "quality": {"columns": ["flag_total_votes_incongruent"]},
                },
                "column_definitions": {
                    "ags": "Eight-digit municipality identifier.",
                    "turnout": "Turnout proportion.",
                },
            }
        },
    }
    catalog_path = tmp_path / "catalog.json"
    catalog_path.write_text(json.dumps(catalog), encoding="utf-8")
    data_dir = tmp_path / "data"
    prepare_dataset(catalog_path, data_dir, "federal_muni_harm_25", source_path)
    return GerdaStore(catalog_path, data_dir), catalog_path, data_dir
