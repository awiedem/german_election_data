#!/usr/bin/env python3
"""Verify the repaired CSV/XLSX against a read-only baseline checkout.

Checks every unchanged CSV line and XLSX row, plus every cell of the 185
affected Excel rows against CSV. RDS equivalence is checked by the R rebuild.
"""
import argparse
import csv
from datetime import datetime
from decimal import Decimal
import hashlib
import itertools
import json
from pathlib import Path
import re
import xml.etree.ElementTree as ET
import zipfile

ROOT = Path(__file__).resolve().parents[2]
STEM = Path("data/state_elections/final/state_unharm")
NS = "http://schemas.openxmlformats.org/spreadsheetml/2006/main"


def sha256(path):
    with path.open("rb") as stream:
        return hashlib.file_digest(stream, "sha256").hexdigest()


def sheet_rows(archive):
    with archive.open("xl/worksheets/sheet1.xml") as stream:
        for _, element in ET.iterparse(stream, events=["end"]):
            if element.tag == f"{{{NS}}}row":
                yield element
                element.clear()


def check(baseline):
    affected = {}
    with (baseline / STEM.with_suffix(".csv")).open("rb") as old, (ROOT / STEM.with_suffix(".csv")).open("rb") as new:
        unchanged = 0
        for n, (left, right) in enumerate(itertools.zip_longest(old, new), 1):
            assert left is not None and right is not None, "CSV length changed"
            values = next(csv.reader([right.decode("utf-8")]))
            if n == 1:
                headers = values
            row = dict(zip(headers, values))
            if row.get("state") == "05" and row.get("election_year") in ("1966", "1970"):
                affected[n] = values
            else:
                assert left == right, f"Unrelated CSV row changed: {n}"
                unchanged += 1
    assert len(affected) == 185
    checked_cells = 0
    with zipfile.ZipFile(baseline / STEM.with_suffix(".xlsx")) as old, zipfile.ZipFile(ROOT / STEM.with_suffix(".xlsx")) as new:
        assert set(old.namelist()) == set(new.namelist())
        for member in new.namelist():
            if member != "xl/worksheets/sheet1.xml":
                assert old.read(member) == new.read(member), f"Excel metadata/style changed: {member}"
        for left, right in itertools.zip_longest(sheet_rows(old), sheet_rows(new)):
            assert left is not None and right is not None, "XLSX row count changed"
            number = int(right.attrib["r"])
            assert left.attrib == right.attrib
            if number not in affected:
                assert ET.tostring(left) == ET.tostring(right), f"Unrelated XLSX row changed: {number}"
                continue
            values = affected[number]
            seen = set()
            for cell in right:
                label = re.match(r"[A-Z]+", cell.attrib["r"])[0]
                column = 0
                for char in label:
                    column = column * 26 + ord(char) - 64
                column -= 1
                seen.add(column)
                expected = values[column]
                value = "".join(cell.itertext())
                assert cell.find(f"{{{NS}}}f") is None
                if headers[column] in ("ags", "state"):
                    assert cell.attrib["t"] == "inlineStr" and value == expected
                elif headers[column] == "election_date":
                    assert Decimal(value) == (datetime.fromisoformat(expected) - datetime(1899, 12, 30)).days
                else:
                    assert Decimal(value) == Decimal(expected), (number, headers[column], value, expected)
                checked_cells += 1
            assert seen == {i for i, v in enumerate(values) if v not in ("", "NA", "NaN")}
    result = {
        "affected_rows": len(affected), "affected_excel_cells_checked": checked_cells,
        "unchanged_csv_and_excel_rows_including_header": unchanged,
        "unrelated_excel_members_identical": True,
        "files": {str(STEM.with_suffix(ext)): {
            "baseline_sha256": sha256(baseline / STEM.with_suffix(ext)),
            "repaired_sha256": sha256(ROOT / STEM.with_suffix(ext))}
            for ext in (".csv", ".rds", ".xlsx")},
    }
    manifest = json.loads((ROOT / "docs/excel_exports.json").read_text())[STEM.as_posix()]
    assert manifest["source_sha256"] == result["files"][str(STEM.with_suffix(".csv"))]["repaired_sha256"]
    assert manifest["rds_sha256"] == result["files"][str(STEM.with_suffix(".rds"))]["repaired_sha256"]
    assert manifest["xlsx_sha256"] == result["files"][str(STEM.with_suffix(".xlsx"))]["repaired_sha256"]
    (ROOT / "data/data_checks/nrw_1966_1970/build_verification.json").write_text(json.dumps(result, indent=2) + "\n")
    print(f"PASS: {len(affected)} complete Excel rows, {checked_cells} cells; "
          f"{unchanged} unrelated CSV/Excel rows (including header) unchanged.")


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("baseline", type=Path)
    check(parser.parse_args().baseline)
