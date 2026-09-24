#!/usr/bin/env python3
"""Check four state workbooks against CSV/manifest, including every invalid count.

Run after code/export_excel.py --only state_unharm state_harm_21 state_harm_23 state_harm_25.
Uses the repository's general XML/style/sampled-value checks as well as a full
row-by-row comparison of invalid_votes, including every missing value and zero.
"""
import csv
from decimal import Decimal
import hashlib
import json
from pathlib import Path
import re
from xml.parsers import expat
import zipfile

from check_excel_exports import verify

ROOT = Path(__file__).resolve().parents[2]
STEMS = ("state_unharm", "state_harm_21", "state_harm_23", "state_harm_25")
EMPTY = {"", "NA", "NaN"}
source_manifest = ROOT / "data/data_checks/state_missingness/source_hashes.csv"
if source_manifest.exists():
    with source_manifest.open(newline="") as f:
        sources = list(csv.DictReader(f))
    for source in sources:
        actual = hashlib.sha256((ROOT / source["path"]).read_bytes()).hexdigest()
        assert actual == source["sha256"], (source["path"], "raw input changed")
    print(f"All {len(sources)} recorded raw input hashes are unchanged.", flush=True)
manifest = json.loads((ROOT / "docs/excel_exports.json").read_text())
reports = []
for stem in STEMS:
    key = f"data/state_elections/final/{stem}"
    source = ROOT / f"{key}.csv"
    report = verify((key, manifest[key], source, None))
    with source.open(newline="") as f:
        reader = csv.reader(f)
        headers = next(reader)
        column = headers.index("invalid_votes")
        expected = [row[column] for row in reader]
    col_letter = ""
    i = column + 1
    while i:
        i, digit = divmod(i - 1, 26)
        col_letter = chr(65 + digit) + col_letter
    state = {"row": 0, "target": False, "value": "", "text": False,
             "seen": False, "checked": 0, "missing": 0, "zero": 0}
    parser = expat.ParserCreate(namespace_separator="|")

    def start(tag, attrs):
        tag = tag.rsplit("|", 1)[-1]
        if tag == "row":
            state.update(row=int(attrs["r"]), seen=False)
        elif tag == "c":
            state.update(target=re.match(r"[A-Z]+", attrs["r"])[0] == col_letter,
                         value="")
        elif tag == "v":
            state["text"] = True

    def text(value):
        if state["target"] and state["text"]:
            state["value"] += value

    def end(tag):
        tag = tag.rsplit("|", 1)[-1]
        if tag == "v":
            state["text"] = False
        elif tag == "c" and state["target"] and state["row"] > 1:
            value = expected[state["row"] - 2]
            assert value not in EMPTY, (stem, state["row"], "invented invalid count")
            assert Decimal(state["value"]) == Decimal(value), (stem, state["row"])
            state["seen"] = True
            state["zero"] += Decimal(value) == 0
        elif tag == "row" and state["row"] > 1:
            value = expected[state["row"] - 2]
            assert state["seen"] == (value not in EMPTY), (stem, state["row"])
            state["checked"] += 1
            state["missing"] += value in EMPTY

    parser.StartElementHandler = start
    parser.CharacterDataHandler = text
    parser.EndElementHandler = end
    with zipfile.ZipFile(ROOT / f"{key}.xlsx") as z:
        with z.open("xl/worksheets/sheet1.xml") as xml:
            for block in iter(lambda: xml.read(1 << 20), b""):
                parser.Parse(block, False)
            parser.Parse(b"", True)
    assert state["checked"] == len(expected)
    report.update(invalid_votes_checked=state["checked"],
                  invalid_votes_missing=state["missing"],
                  invalid_votes_zero=state["zero"])
    reports.append(report)
out = ROOT / "data/data_checks/state_missingness/excel_checks.json"
out.parent.mkdir(parents=True, exist_ok=True)
out.write_text(json.dumps(reports, indent=2) + "\n")
print("Every invalid_votes value and missing cell matches CSV in all four workbooks.")
