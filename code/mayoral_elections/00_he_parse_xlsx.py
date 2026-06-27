#!/usr/bin/env python3
"""Stage-0 parser for the Hessisches Statistisches Landesamt report
"B VII m – Direktwahlen", **May 2026 issue, XLSX format**
(`data/mayoral_elections/raw/hessen/HE_Direktwahlen_BVII-m05-2026_StatHessen.xlsx`,
sheet 'Direktwahlen'). This is a most-recent-per-Gemeinde/Landkreis snapshot of the
last direct Bürgermeister-/Oberbürgermeister-/Landratswahl in Hessen (Wahltage
~2020-2026), and it SUPERSEDES the older 2024 PDF parse (00_he_parse.py).

Why it is a major upgrade over the 2024 PDF (he_pdf_parsed.csv):
  * **Every Wahlvorschlag** is listed (party + votes + %), not just the winner +
    first Wahlvorschlag -> real all-candidate data and a real `n_candidates`.
  * **Full vote counts** (Wahlberechtigte / Wähler / ungültige / gültige Stimmen),
    not only the winner's votes.

Column layout of the 'Direktwahlen' sheet (0-indexed; verified):
  0 AGS (3-digit = Landkreis, 6-digit = Gemeinde; Hanau is coded 415000)
  1 Landkreis/Gemeinde name        2 'Stichwahl' marker (else Hauptwahl)
  3 Wahltag   4 Wahlberechtigte    5 Wahlbeteiligung %   6 Wähler/innen
  7 ungültige Stimmen   8 ungültige %   9 gültige Stimmen
  10 Gewählte/r: Name   11 Träger des Wahlvorschlags   12 Geschlecht (m/w)
  13.. Wahlvorschläge in triplets (Träger, Anzahl, %) -> up to 20 Wahlvorschläge.

Winner detection (verified bulletproof, 0 mismatches over 384 decisive rows): on a
row whose "Gewählte/r" name is non-empty, the winner is the **max-votes Wahlvorschlag**
(NOT a party-string match — 50 rows have several Einzelbewerbungen sharing a Träger);
its Träger always equals column 11 (asserted). A Hauptwahl that went to a Stichwahl
has an empty "Gewählte/r" cell (no winner on that row) and its own Stichwahl row.

Merge: units the XLSX omits (their last direct election predates its 2020-2026
window, ~57 units) are kept from the 2024 PDF parse `he_pdf_parsed.csv`, so coverage
never shrinks. Output = the final `he_parsed.csv` consumed by stages 01/01b.
Run order:  python3 00_he_parse.py   (writes he_pdf_parsed.csv)
            python3 00_he_parse_xlsx.py   (writes he_parsed.csv)
"""

import csv
import os
import sys

import openpyxl

HERE = os.path.dirname(os.path.abspath(__file__))
ROOT = os.path.dirname(os.path.dirname(HERE))
RAW_DIR = os.path.join(ROOT, "data", "mayoral_elections", "raw", "hessen")
XLSX = os.path.join(RAW_DIR, "HE_Direktwahlen_BVII-m05-2026_StatHessen.xlsx")
PDF_CSV = os.path.join(RAW_DIR, "he_pdf_parsed.csv")   # 2024 fallback
OUT = os.path.join(RAW_DIR, "he_parsed.csv")

STATE, STATE_NAME = "06", "Hessen"

OB_KREISFREI = {"06411000", "06412000", "06413000", "06414000", "06611000"}
OB_SONDERSTATUS = {
    "06433012",  # Rüsselsheim am Main
    "06434001",  # Bad Homburg v. d. Höhe
    "06435014",  # Hanau
    "06531005",  # Gießen
    "06532023",  # Wetzlar
    "06534014",  # Marburg
    "06631009",  # Fulda
}
OB_ALL = OB_KREISFREI | OB_SONDERSTATUS
# the report codes Hanau as 415000 (pseudo-kreisfrei) instead of its real AGS
AGS_OVERRIDE = {"415000": "06435014"}

FIELDS = ["ags", "ags_name", "state", "state_name", "election_year", "election_date",
          "election_type", "round", "eligible_voters", "number_voters", "valid_votes",
          "invalid_votes", "turnout", "candidate_name", "candidate_last_name",
          "candidate_first_name", "candidate_gender", "candidate_party",
          "candidate_votes", "candidate_voteshare", "is_winner", "candidate_rank",
          "n_candidates", "source_file"]


def num(x):
    if x is None:
        return None
    try:
        return float(str(x).replace(",", "."))
    except ValueError:
        return None


def to_int(x):
    v = num(x)
    return int(round(v)) if v is not None else None


def std_ags(raw):
    raw = str(raw).strip()
    if raw in AGS_OVERRIDE:
        return AGS_OVERRIDE[raw]
    if len(raw) == 3:                 # Landkreis -> 8-digit Kreis code
        return "06" + raw + "000"
    return "06" + raw                 # Gemeinde (6-digit)


def classify(ags, raw):
    if len(str(raw).strip()) == 3:
        return "Landratswahl"
    if ags in OB_ALL:
        return "Oberbürgermeisterwahl"
    return "Bürgermeisterwahl"


def gender(g):
    g = str(g or "").strip().lower()
    return {"m": "m", "w": "w", "f": "w"}.get(g, "")


def split_name(full):
    """Report names are 'Lastname, Firstname' (incl. titles like 'Dr. ')."""
    full = " ".join(str(full or "").split())
    if "," in full:
        last, first = full.split(",", 1)
        return full, last.strip(), first.strip()
    return full, full, ""


def parse_xlsx():
    wb = openpyxl.load_workbook(XLSX, read_only=True, data_only=True)
    rows = [r for r in wb["Direktwahlen"].iter_rows(values_only=True)][4:]  # skip 4 header rows
    out, xlsx_ags = [], set()
    n_assert = 0
    for r in rows:
        if r[0] is None:
            continue
        raw = str(r[0]).strip()
        ags = std_ags(raw)
        xlsx_ags.add(ags)
        etype = classify(ags, raw)
        rnd = "stichwahl" if "stichwahl" in str(r[2] or "").lower() else "hauptwahl"
        wahltag = str(r[3])[:10] if r[3] is not None else ""
        valid = to_int(r[9])
        # collect Wahlvorschläge: triplets from col 13
        wvs = []
        j = 13
        while j + 1 < len(r):
            party = str(r[j] or "").strip()
            votes = to_int(r[j + 1])
            if votes is not None:
                wvs.append({"party": party, "votes": votes})
            j += 3
        if not wvs:
            continue
        ncand = len(wvs)
        # winner = max-votes Wahlvorschlag, but only on a decisive row (named winner)
        gew_name = str(r[10] or "").strip()
        win = max(wvs, key=lambda w: w["votes"]) if gew_name else None
        if win is not None:
            assert win["party"] == str(r[11] or "").strip(), \
                f"winner party mismatch at {raw} {r[1]}: {win['party']} != {r[11]}"
            n_assert += 1
        ranked = sorted(wvs, key=lambda w: -w["votes"])
        rank_of = {id(w): i + 1 for i, w in enumerate(ranked)}
        full, last, first = split_name(gew_name)
        for w in wvs:
            is_w = win is not None and w is win
            out.append({
                "ags": ags, "ags_name": str(r[1] or "").strip(),
                "state": STATE, "state_name": STATE_NAME,
                "election_year": wahltag[:4], "election_date": wahltag,
                "election_type": etype, "round": rnd,
                "eligible_voters": to_int(r[4]) if to_int(r[4]) is not None else "",
                "number_voters": to_int(r[6]) if to_int(r[6]) is not None else "",
                "valid_votes": valid if valid is not None else "",
                "invalid_votes": to_int(r[7]) if to_int(r[7]) is not None else "",
                "turnout": round(num(r[5]) / 100, 6) if num(r[5]) is not None else "",
                "candidate_name": full if is_w else "",
                "candidate_last_name": last if is_w else "",
                "candidate_first_name": first if is_w else "",
                "candidate_gender": gender(r[12]) if is_w else "",
                "candidate_party": w["party"],
                "candidate_votes": w["votes"],
                "candidate_voteshare": round(w["votes"] / valid, 6) if valid else "",
                "is_winner": "TRUE" if is_w else "FALSE",
                "candidate_rank": rank_of[id(w)],
                "n_candidates": ncand,
                "source_file": "HE_Direktwahlen_BVII-m05-2026_StatHessen.xlsx",
            })
    return out, xlsx_ags, n_assert


def main():
    if not os.path.exists(XLSX):
        raise SystemExit(f"XLSX not found: {XLSX}")
    rows, xlsx_ags, n_assert = parse_xlsx()

    # merge in the 2024-PDF fallback for AGS the XLSX does not cover
    pdf_added, pdf_units = 0, set()
    if os.path.exists(PDF_CSV):
        with open(PDF_CSV, encoding="utf-8") as fh:
            for r in csv.DictReader(fh):
                if r["ags"] in xlsx_ags:
                    continue
                pdf_units.add(r["ags"])
                rows.append({**{k: "" for k in FIELDS}, **{k: r.get(k, "") for k in FIELDS if k in r}})
                # ensure n_candidates present (PDF has none -> NA)
                rows[-1].setdefault("n_candidates", "")
                pdf_added += 1

    from collections import Counter
    by_type = Counter(x["election_type"] for x in rows)
    n_win = sum(1 for x in rows if x["is_winner"] == "True")
    sys.stderr.write(
        f"\n=== Hessen Direktwahlen B VII m (May 2026 XLSX) ===\n"
        f"  XLSX rows: {len(rows) - pdf_added}  ({len(xlsx_ags)} units, "
        f"{n_assert} winners; max {max(int(x['n_candidates']) for x in rows if x['n_candidates']) } Wahlvorschläge)\n"
        f"  PDF-fallback rows: {pdf_added}  ({len(pdf_units)} units not in the 2026 XLSX)\n"
        f"  total rows: {len(rows)}  winners: {n_win}\n"
        f"  by type: {dict(by_type)}\n")

    with open(OUT, "w", newline="", encoding="utf-8") as fh:
        w = csv.DictWriter(fh, fieldnames=FIELDS)
        w.writeheader()
        w.writerows(rows)
    sys.stderr.write(f"  Wrote {len(rows)} rows -> {OUT}\n")


if __name__ == "__main__":
    main()
