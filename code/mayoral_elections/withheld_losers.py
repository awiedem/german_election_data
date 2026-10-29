"""Blank the names of losing candidates that GERDA withholds in every file.

Stage-0 scrapers call withhold() on their rows just before writing. The pins in
metadata/withheld_loser_rows.csv are keyed on counts, never on names, so the
tracked pin file names nobody. Each pin must match exactly one losing row.
"""
import csv
import os

ROOT = os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
PINS = os.path.join(ROOT, "data", "mayoral_elections", "metadata", "withheld_loser_rows.csv")
NAME_FIELDS = ["candidate_name", "candidate_last_name", "candidate_first_name"]


def withhold(rows, source):
    with open(PINS, encoding="utf-8") as fh:
        pins = [p for p in csv.DictReader(fh) if p["source"] == source]
    for p in pins:
        hits = [r for r in rows if r["ags"] == p["ags"] and r["election_date"] == p["election_date"]
                and r["round"] == p["round"]
                and (not p["candidate_votes"] or str(r["candidate_votes"]) == p["candidate_votes"])
                and (not p["candidate_voteshare"]
                     or abs(float(r["candidate_voteshare"]) - float(p["candidate_voteshare"])) < 1e-9)]
        if len(hits) != 1 or str(hits[0]["is_winner"]).upper() == "TRUE":
            raise SystemExit(f"FATAL: withheld-loser pin does not match one losing row: {p}")
        for f in NAME_FIELDS:
            hits[0][f] = ""
    return rows
