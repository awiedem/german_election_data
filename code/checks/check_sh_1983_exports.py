#!/usr/bin/env python3
"""Check every SH 1983 Excel cell against the rebuilt CSV and export hashes."""
import csv
from datetime import datetime
from decimal import Decimal
import hashlib
import json
from pathlib import Path
import re
import xml.etree.ElementTree as ET
import zipfile

ROOT = Path(__file__).resolve().parents[2]
STEM = "data/state_elections/final/state_unharm"
NS = "http://schemas.openxmlformats.org/spreadsheetml/2006/main"
EMPTY = {"", "NA", "NaN"}


def sha256(path):
    h = hashlib.sha256()
    with path.open("rb") as stream:
        for block in iter(lambda: stream.read(1 << 20), b""):
            h.update(block)
    return h.hexdigest()


def main():
    manifest = json.loads((ROOT / "docs/excel_exports.json").read_text())[STEM]
    hashes = {ext: sha256(ROOT / (STEM + "." + ext)) for ext in ("csv", "rds", "xlsx")}
    for ext, key in (("csv", "source_sha256"), ("rds", "rds_sha256"), ("xlsx", "xlsx_sha256")):
        assert hashes[ext] == manifest[key], f"Stale export: {ext}"
    affected = {}
    with (ROOT / (STEM + ".csv")).open(newline="") as stream:
        reader = csv.reader(stream)
        header = next(reader)
        year, state = header.index("election_year"), header.index("state")
        for number, values in enumerate(reader, 2):
            if values[state] == "01" and values[year] == "1983":
                affected[number] = values
    assert len(affected) == 1128 and number == manifest["rows"] + 1
    cells_checked = 0
    rows_checked = 0
    with zipfile.ZipFile(ROOT / (STEM + ".xlsx")) as archive:
        styles = ET.fromstring(archive.read("xl/styles.xml"))
        formats = {int(x.attrib["numFmtId"]): x.attrib["formatCode"]
                   for x in styles.findall(f"{{{NS}}}numFmts/{{{NS}}}numFmt")}
        cell_formats = [formats.get(int(x.attrib["numFmtId"]), "General")
                        for x in styles.find(f"{{{NS}}}cellXfs")]
        with archive.open("xl/worksheets/sheet1.xml") as stream:
            for _, row in ET.iterparse(stream, events=["end"]):
                if row.tag != f"{{{NS}}}row":
                    continue
                rows_checked += 1
                number = int(row.attrib["r"])
                if number in affected:
                    values = affected[number]
                    seen = set()
                    for cell in row:
                        column = 0
                        for letter in re.match("[A-Z]+", cell.attrib["r"])[0]:
                            column = column * 26 + ord(letter) - 64
                        column -= 1
                        seen.add(column)
                        value = "".join(cell.itertext())
                        expected = values[column]
                        assert cell.find(f"{{{NS}}}f") is None
                        if header[column] in ("ags", "state"):
                            assert cell.attrib["t"] == "inlineStr" and value == expected
                        elif header[column] == "election_date":
                            assert Decimal(value) == (datetime.fromisoformat(expected) - datetime(1899, 12, 30)).days
                        else:
                            assert Decimal(value) == Decimal(expected), (number, header[column])
                        if header[column] in manifest["percentage_columns"]:
                            assert cell_formats[int(cell.attrib["s"])] == "0.00%"
                        cells_checked += 1
                    assert seen == {i for i, value in enumerate(values) if value not in EMPTY}
                    assert values[header.index("turnout")] in EMPTY
                row.clear()
    assert rows_checked == manifest["rows"] + 1
    result = dict(sh_rows=1128, excel_cells_checked=cells_checked,
                  total_rows=manifest["rows"], columns=manifest["columns"],
                  source_and_export_hashes=hashes, missing_turnout_preserved=True,
                  identifiers_stored_as_text=True)
    (ROOT / "data/data_checks/sh_1983/export_verification.json").write_text(json.dumps(result, indent=2) + "\n")
    print(f"PASS: 1,128 SH Excel rows, {cells_checked:,} cells, text identifiers, missing turnout and export hashes.")


if __name__ == "__main__":
    main()
