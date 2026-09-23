#!/usr/bin/env python3
"""Validate the source-checked Table 2 transcription and build NRW counts.

No OCR inference, percentage reconstruction, rescaling, or raw-file writes.
See docs/nrw_1966_1970_repair.md for the transcription and geographic conventions.
"""
import argparse
import csv
import hashlib
import json
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
DERIVED = ROOT / "data/state_elections/derived/nrw_1966_1970"
CHECKS = ROOT / "data/data_checks/nrw_1966_1970"
PARTIES = ["cdu", "spd", "fdp", "zentrum", "uap", "fsu", "dkp", "npd"]
COUNTS = ["eligible_voters", "number_voters", "invalid_votes", "valid_votes"] + PARTIES
# Independently read from the statewide a-rows, physical pp. 32/33 and 34/35.
STATE_TOTALS = {
    1966: [11292041, 8641646, 99153, 8542493,
           3653184, 4226604, 633765, 16181, 3175, 9584, 0, 0],
    1970: [11890609, 8739772, 61945, 8677827,
           4020186, 3996808, 478420, 9902, 1504, 0, 76964, 94043],
}
REGION_TOTALS = {1966: [24, 34, 43, 60, 75, 101],
                 1970: [23, 34, 45, 64, 80, 106]}


def require(condition, message):
    if not condition:
        raise ValueError(message)


def write_table(path, rows, delimiter=","):
    path.parent.mkdir(parents=True, exist_ok=True)
    with path.open("w", encoding="utf-8", newline="") as stream:
        writer = csv.DictWriter(stream, fieldnames=list(rows[0]),
                                delimiter=delimiter, lineterminator="\n")
        writer.writeheader()
        writer.writerows(rows)


def load_verified():
    with (DERIVED / "table2_transcription.tsv").open(encoding="utf-8") as stream:
        rows = list(csv.DictReader(stream, delimiter="\t"))
    for row in rows:
        for field in ["election_year", "lfd_nr", "pdf_left", "pdf_right",
                      "printed_left", "printed_right"] + COUNTS:
            require(row[field].isdigit(), f"Missing/noninteger {field}: {row}")
            row[field] = int(row[field])
    diagnostics = []

    def compare(year, kind, nr, field, observed, expected):
        diagnostics.append(dict(election_year=year, check=kind, lfd_nr=nr,
                                field=field, observed=observed, expected=expected,
                                difference=observed - expected))
        require(observed == expected,
                f"{year} {kind} row {nr} {field}: {observed} != {expected}")

    for year in STATE_TOTALS:
        subset = [r for r in rows if r["election_year"] == year]
        require([r["lfd_nr"] for r in subset] == list(range(1, 104 if year == 1966 else 112)),
                f"Incomplete, duplicate or reordered source rows: {year}")
        units = [r for r in subset if r["type"] != "agg"]
        require(len(units) == (95 if year == 1966 else 90), f"Unit count: {year}")
        for r in subset:
            nr = r["lfd_nr"]
            require(r["type"] in ("krfr", "kreis", "agg") and r["source_row"] == "a",
                    f"Wrong row semantics: {year} {nr}")
            require(0 < r["number_voters"] <= r["eligible_voters"], f"Turnout: {year} {nr}")
            require(r["pdf_right"] == r["pdf_left"] + 1 and
                    r["printed_left"] == r["pdf_left"] - 2 and
                    r["printed_right"] == r["pdf_right"] - 2,
                    f"Page reference: {year} {nr}")
            compare(year, "ballot_identity", nr, "number_voters",
                    r["valid_votes"] + r["invalid_votes"], r["number_voters"])
            compare(year, "party_identity", nr, "valid_votes",
                    sum(r[p] for p in PARTIES), r["valid_votes"])
        # Subtotals are independently transcribed, never computed as inputs.
        controls = []
        for nr in REGION_TOTALS[year]:
            target = subset[nr - 1]
            members = [r for r in units if r["region"] == target["region"]]
            controls.append((target, members))
            if year == 1970:
                for offset, typ in [(1, "krfr"), (2, "kreis")]:
                    controls.append((subset[nr - 1 + offset],
                                     [r for r in members if r["type"] == typ]))
        state_nr = 102 if year == 1966 else 109
        controls.append((subset[state_nr - 1], units))
        for offset, typ in ([(1, "kreis")] if year == 1966 else [(1, "krfr"), (2, "kreis")]):
            controls.append((subset[state_nr - 1 + offset], [r for r in units if r["type"] == typ]))
        for target, members in controls:
            for field in COUNTS:
                compare(year, "source_subtotal", target["lfd_nr"], field,
                        sum(r[field] for r in members), target[field])
        for field, expected in zip(COUNTS, STATE_TOTALS[year]):
            compare(year, "statewide_control", state_nr, field,
                    sum(r[field] for r in units), expected)
    require(len(rows) == 214, "Unexpected election rows")
    return rows, diagnostics


def build(source_root):
    sources = json.loads((DERIVED / "sources.json").read_text())
    for source in sources.values():
        path = source_root / source["file"]
        require(hashlib.sha256(path.read_bytes()).hexdigest() == source["sha256"],
                f"Source hash mismatch (possibly an LFS pointer): {path}")
    rows, diagnostics = load_verified()
    # Preserve the published IDs, which followed the legacy CSV's name order,
    # not the printed table order. Never renumber units during a count repair.
    with (DERIVED / "unit_identifiers.tsv").open(encoding="utf-8") as stream:
        identifiers = list(csv.DictReader(stream, delimiter="\t"))
    source_units = {(r["election_year"], r["lfd_nr"]): r for r in rows if r["type"] != "agg"}
    require(len(identifiers) == len(source_units) == 185, "Incomplete identifier map")
    units = []
    seen = set()
    for identifier in identifiers:
        key = int(identifier["election_year"]), int(identifier["lfd_nr"])
        require(key not in seen, f"Duplicate identifier mapping: {key}")
        seen.add(key)
        row = source_units[key]
        require((row["name"], row["type"]) == (identifier["name"], identifier["type"]),
                f"Identifier/source unit mismatch: {key}")
        units.append(dict(ags=identifier["ags"], **row))
    write_table(DERIVED / "nrw_1966_1970_kreis.csv", units)
    write_table(CHECKS / "source_reconciliation.tsv", diagnostics, "\t")
    print(f"Validated {len(rows)} source rows, {len(units)} units, "
          f"{len(diagnostics)} exact reconciliation checks.")


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--source-root", type=Path, default=ROOT / "data/state_elections/raw/Landtagswahlen/Nordrhein-Westfalen")
    build(parser.parse_args().source_root)
