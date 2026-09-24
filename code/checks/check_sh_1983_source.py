#!/usr/bin/env python3
"""Validate SH 1983 against source controls; never modify raw files.

Run from any directory. Requires only Python's standard library.
"""
import copy
import csv
import hashlib
import json
from collections import Counter
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
DERIVED = ROOT / "data/state_elections/derived/sh_1983"


def read_tsv(path):
    with path.open(newline="") as stream:
        return list(csv.DictReader(stream, delimiter="\t"))


def require(condition, message):
    if not condition:
        raise ValueError(message)


def validate(rows, controls, percentages, expected_ids):
    require(len(rows) == controls["rows"], "Missing/extra municipal records")
    ids = [row["ags"] for row in rows]
    require(len(set(ids)) == len(ids), "Duplicate identifier")
    require(set(ids) == expected_ids, "Municipality identifiers differ from 1983 registry")
    fields = list(controls["in_person_totals"])
    parties = fields[6:]
    numeric = {}
    for row in rows:
        require(all(row[k].isdigit() for k in fields), "Missing or invalid count")
        v = {k: int(row[k]) for k in fields}
        require(v["eligible_voters"] > 0 and v["valid_votes"] > 0, "Nonpositive electorate/votes")
        require(v["eligible_voters"] == v["without_certificate"] + v["with_certificate"], "Electorate components")
        require(v["number_voters"] == v["valid_votes"] + v["invalid_votes"], "Voter/ballot identity")
        require(v["valid_votes"] == sum(v[k] for k in parties), "Party/valid-vote identity")
        numeric[row["ags"]] = v
    for field, total in controls["in_person_totals"].items():
        require(sum(v[field] for v in numeric.values()) == total, f"Printed total: {field}")
    for county, total in controls["county_electorate"].items():
        require(sum(v["eligible_voters"] for ags, v in numeric.items() if ags[3:5] == county) == total,
                f"County electorate: {county}")
    require(dict(Counter(ags[3:5] for ags in ids)) == controls["county_rows"], "County row counts")
    for row in percentages:
        v = numeric[row["ags"]]
        field = row["field"]
        denominator = (v["valid_votes"] if field in parties else
                       v["number_voters"] if field == "invalid_votes" else v["eligible_voters"])
        require(abs(100 * v[field] / denominator - float(row["printed_percentage"])) <= 0.051,
                f"Printed percentage: {row['ags']}/{field}")
    for field, postal in controls["postal_totals"].items():
        require(controls["in_person_totals"][field] + postal == controls["overall_totals"][field],
                f"Postal reconciliation: {field}")
    eb = sum(v["einzelbewerber_llsh"] for ags, v in numeric.items() if ags[3:5] in controls["independent_counties"])
    require(eb == controls["in_person_independents"] == 50, "Independent candidate attribution")
    require(sum(v["einzelbewerber_llsh"] for v in numeric.values()) - eb == controls["in_person_llsh"] == 94,
            "LLSH attribution")


def main():
    controls = json.loads((DERIVED / "sources.json").read_text())
    for item in controls["sources"]:
        require(hashlib.sha256((ROOT / item["path"]).read_bytes()).hexdigest() == item["sha256"],
                f"Changed raw source: {item['path']}")
    for filename, expected in controls["files"].items():
        require(hashlib.sha256((DERIVED / filename).read_bytes()).hexdigest() == expected,
                f"Changed reviewed transcription/evidence: {filename}")
    registry = ROOT / controls["sources"][1]["path"]
    with registry.open(encoding="latin1", newline="") as stream:
        ids = {"01" + row["Kreis"].zfill(3) + row["Gemeinde"]
               for row in csv.DictReader(stream, delimiter=";") if row["Land"] == "01" and row["BA"] == "0"}
    rows = read_tsv(DERIVED / "table6_transcription.tsv")
    percentages = read_tsv(DERIVED / "percentage_checks.tsv")
    validate(rows, controls, percentages, ids)

    # Regressions for failures in the old extractor: lost rows, fake codes,
    # dropped leading digits, missing -> zero, and balanced party-column shifts.
    variants = []
    variants.append(rows[:-1])
    bad = copy.deepcopy(rows); bad[0]["ags"] = "01001075"; variants.append(bad)
    bad = copy.deepcopy(rows); bad[0]["number_voters"] = "9407"; variants.append(bad)
    bad = copy.deepcopy(rows); bad[0]["eligible_voters"] = "0"; variants.append(bad)
    bad = copy.deepcopy(rows); bad[0]["cdu"], bad[0]["spd"] = bad[0]["spd"], bad[0]["cdu"]; variants.append(bad)
    # Preserve row and statewide party sums: only printed percentages detect this.
    bad = copy.deepcopy(rows)
    for index, sign in [(0, 1), (1, -1)]:
        bad[index]["cdu"] = str(int(bad[index]["cdu"]) + sign * 1000)
        bad[index]["spd"] = str(int(bad[index]["spd"]) - sign * 1000)
    variants.append(bad)
    for bad in variants:
        try:
            validate(bad, controls, percentages, ids)
        except ValueError:
            pass
        else:
            raise AssertionError("Corrupt transcription escaped validation")
    print(f"PASS: {len(rows):,} municipalities, all source totals, county electorate, "
          f"{len(percentages):,} printed percentages, source hashes and six corruption regressions.")


if __name__ == "__main__":
    main()
