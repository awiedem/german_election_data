#!/usr/bin/env python3
"""Stage-0 parser for the Sachsen-Anhalt (ST) Statistisches Landesamt
Bürgermeister-/Oberbürgermeisterwahlen dataset.

Source: Statistisches Landesamt Sachsen-Anhalt, Dezernat 13
  data/mayoral_elections/raw/sachsen_anhalt/bmbm.csv    (Windows-1252, CRLF, ';')
  data/mayoral_elections/raw/sachsen_anhalt/DSB_bmbm.pdf (Datensatzbeschreibung)

Layout — one wide row per Gemeinde, "most recent Bürgermeisterwahl" snapshot as
of the extract date (26.02.2025 per DSB), covering elections 2019-11-10 through
2026-02-15 (7-year Amtszeiten → all currently-serving Mayors' most recent
election). ~218 Gemeinden, matches the ST hauptamtliche-Gemeinde universe.

Columns (per DSB_bmbm.pdf):
  ERGART, ERGART_SW           Ergebnisart (E = endgültig; leer = SW nicht statt-
                              gefunden)
  WDATUM, WDATUM_SW           Wahldatum HW / SW (tt.mm.jjjj)
  KREIS                       5-digit Kreisschlüssel
  GEMNR                       8-digit AGS (Gemeindeschlüssel)
  GNAME                       Gemeindename
  ART                         Art des Amtes:
                                HaOB = hauptamtl. Oberbürgermeister
                                Ha   = hauptamtl. Bürgermeister
                                leer = ehrenamtl. Bürgermeister
  TITEL, NAME, VORNAME, PARTEI  Elected mayor
  WBER..GUESTI (HW/SW)        Wahlberechtigte/Wähler/Ungültige/Gültige
  ANZBEW                      Anzahl Bewerber
  B01..B15 * (TITEL/NAME/VORNAM/PARTEI/STI/STI_SW)  15 candidate slots

Quirks / caveats:
  * Windows-1252 encoding.
  * German numbers use a NON-BREAKING SPACE thousands-separator ("87 437").
  * PARTEI fields are TRUNCATED at 20 chars — long coalitions read as e.g.
    "SPD, DIE LINKE, GRÜN" (missing final E). Emitted verbatim; downstream
    normalise_party is responsible for reconciling.
  * ~65% of winners (142/218) have empty PARTEI → Einzelbewerber. Empty
    candidate PARTEI (B{i}) means Einzelbewerber, NOT missing data.
  * Uncontested elections (ANZBEW=1) are Ja/Nein-style — the single candidate's
    B01_STI equals GUESTI; Ungültige effectively counts Nein.
  * Some ehrenamtl. Gemeinden had no Stichwahl regardless of majority: for
    single-round rows WDATUM_SW / all *_SW fields / B{i}_STI_SW are blank.

Output (candidate-level long, one row per candidate per round; schema matches
the existing portal file `st_bm_parsed.csv` so stages 01/01b consume it
identically):
  data/mayoral_elections/raw/sachsen_anhalt/st_stala_parsed.csv

The portal scraper (00_st_scrape.py) remains as-is: it writes
`st_bm_parsed.csv`, which is used ONLY as a fallback for (ags, election_date)
pairs missing from the StaLA snapshot (e.g. post-cutoff 2026 elections).
Stages 01/01b merge the two, StaLA primary.

Run:  python3 code/mayoral_elections/00_st_stala_parse.py
"""

import csv
import os
import re
import sys
from typing import Optional

HERE = os.path.dirname(os.path.abspath(__file__))
ROOT = os.path.dirname(os.path.dirname(HERE))
RAW_DIR = os.path.join(ROOT, "data", "mayoral_elections", "raw", "sachsen_anhalt")
IN_CSV = os.path.join(RAW_DIR, "bmbm.csv")
OUT_CSV = os.path.join(RAW_DIR, "st_stala_parsed.csv")

STATE = "15"
STATE_NAME = "Sachsen-Anhalt"
N_CAND_SLOTS = 15
SOURCE_TAG = "StaLA bmbm.csv (Dezernat 13)"

DATE_RE = re.compile(r"^\s*(\d{2})\.(\d{2})\.(\d{4})\s*$")


def strip_nbsp(s: str) -> str:
    """Trim + collapse whitespace incl. NO-BREAK SPACE (U+00A0)."""
    if s is None:
        return ""
    # replace NBSP and thin space with regular space
    return s.replace(" ", " ").replace(" ", " ").strip()


def clean_num(s: str) -> Optional[float]:
    """Parse a German integer that may contain space/nbsp thousands separators.

    Returns None for empty / non-numeric strings. Emits int-valued float so the
    CSV round-trips through R fread cleanly.
    """
    s = strip_nbsp(s).replace(" ", "")
    if not s:
        return None
    # DSB dictates integers; still handle a stray comma-decimal just in case
    s = s.replace(",", ".")
    try:
        v = float(s)
    except ValueError:
        return None
    return v


def parse_date(s: str) -> Optional[str]:
    """`tt.mm.jjjj` → `yyyy-mm-dd`, or None if empty/malformed."""
    m = DATE_RE.match(s or "")
    if not m:
        return None
    dd, mm, yy = m.groups()
    return f"{yy}-{mm}-{dd}"


def classify_election_type(art: str) -> str:
    """DSB dictionary:
       HaOB → Oberbürgermeisterwahl
       Ha   → Bürgermeisterwahl (hauptamtlich)
       leer → Bürgermeisterwahl (ehrenamtlich)
    Both hauptamtl. and ehrenamtl. mayors are Bürgermeisterwahlen — the
    hauptamtlich vs. ehrenamtlich distinction is not encoded in election_type
    (all downstream ST pipelines pool them, consistent with other states)."""
    art = strip_nbsp(art)
    if art == "HaOB":
        return "Oberbürgermeisterwahl"
    # "Ha" or "" (ehrenamtlich)
    return "Bürgermeisterwahl"


def compute_turnout(waehler, wber) -> Optional[float]:
    if waehler is None or wber is None or wber == 0:
        return None
    return waehler / wber


def compute_share(votes, gueltig) -> Optional[float]:
    if votes is None or gueltig is None or gueltig == 0:
        return None
    return votes / gueltig


def main():
    if not os.path.exists(IN_CSV):
        sys.exit(f"missing input: {IN_CSV}")

    with open(IN_CSV, "r", encoding="cp1252", newline="") as fh:
        reader = csv.reader(fh, delimiter=";")
        header = next(reader)
        rows = list(reader)

    # Verify header shape once — 21 metadata cols + 15×6 candidate cols = 111
    expected_ncol = 21 + N_CAND_SLOTS * 6
    if len(header) != expected_ncol:
        sys.exit(f"unexpected column count: {len(header)} (want {expected_ncol})")

    IDX = {name: i for i, name in enumerate(header)}
    # Sanity: required columns from DSB
    for req in ["ERGART", "WDATUM", "GEMNR", "GNAME", "ART", "NAME",
                "WBER", "WAEHLER", "UNGSTI", "GUESTI", "ANZBEW",
                "B01_NAME", "B01_STI", "B15_STI_SW"]:
        if req not in IDX:
            sys.exit(f"column missing: {req}")

    out_rows: list[dict] = []

    # counters for diagnostics
    n_hw = n_sw = 0
    n_by_office = {"Oberbürgermeisterwahl": 0, "Bürgermeisterwahl": 0}
    n_sum_mismatch_hw = n_sum_mismatch_sw = 0

    for row_idx, r in enumerate(rows, start=2):  # start=2 for line number
        # Pad short rows just in case (shouldn't happen with proper CSV)
        if len(r) < expected_ncol:
            r = r + [""] * (expected_ncol - len(r))

        gemnr = strip_nbsp(r[IDX["GEMNR"]])
        gname = strip_nbsp(r[IDX["GNAME"]])
        if not gemnr:
            continue  # skip empty rows

        if len(gemnr) != 8 or not gemnr.isdigit():
            sys.exit(f"bad AGS at row {row_idx}: {gemnr!r}")
        if not gemnr.startswith(STATE):
            sys.exit(f"non-ST AGS at row {row_idx}: {gemnr!r}")

        hw_date = parse_date(r[IDX["WDATUM"]])
        sw_date = parse_date(r[IDX["WDATUM_SW"]])
        if hw_date is None:
            sys.exit(f"missing HW date at row {row_idx}: {gname}")

        art = strip_nbsp(r[IDX["ART"]])
        election_type = classify_election_type(art)
        n_by_office[election_type] += 1

        election_year = int(hw_date[:4])

        # Turnout aggregates (per round)
        wber_hw    = clean_num(r[IDX["WBER"]])
        waehler_hw = clean_num(r[IDX["WAEHLER"]])
        ungsti_hw  = clean_num(r[IDX["UNGSTI"]])
        guesti_hw  = clean_num(r[IDX["GUESTI"]])

        wber_sw    = clean_num(r[IDX["WBER_SW"]])
        waehler_sw = clean_num(r[IDX["WAEHLER_SW"]])
        ungsti_sw  = clean_num(r[IDX["UNGSTI_SW"]])
        guesti_sw  = clean_num(r[IDX["GUESTI_SW"]])

        anzbew = clean_num(r[IDX["ANZBEW"]])
        anzbew = int(anzbew) if anzbew is not None else 0

        # Elected mayor identity (for cross-check + is_winner assignment
        # downstream if we ever want it)
        winner_titel  = strip_nbsp(r[IDX["TITEL"]])
        winner_last   = strip_nbsp(r[IDX["NAME"]])
        winner_first  = strip_nbsp(r[IDX["VORNAME"]])
        winner_partei = strip_nbsp(r[IDX["PARTEI"]])

        # Collect candidate slots B01..B15. A candidate slot is legitimate iff
        # it has HW votes recorded (`sti_hw` is not None). Slots with no votes
        # but stray metadata are dropped with a warning — this catches the
        # known Genthin 2024 corruption (row 118 of the extract dated 26.02.2025),
        # where B08+B09 hold garbage tokens "AfD;m;185" / "w;179" that are NOT
        # a real candidate (the missing 8th candidate — Wöhling, 96 votes —
        # is only recoverable from the Landeswahlleiter portal scrape).
        cands = []
        for k in range(1, N_CAND_SLOTS + 1):
            titel  = strip_nbsp(r[IDX[f"B{k:02d}_TITEL"]])
            last   = strip_nbsp(r[IDX[f"B{k:02d}_NAME"]])
            first  = strip_nbsp(r[IDX[f"B{k:02d}_VORNAM"]])
            partei = strip_nbsp(r[IDX[f"B{k:02d}_PARTEI"]])
            sti_hw = clean_num(r[IDX[f"B{k:02d}_STI"]])
            sti_sw = clean_num(r[IDX[f"B{k:02d}_STI_SW"]])
            has_meta = any([titel, last, first, partei])
            has_votes = sti_hw is not None or sti_sw is not None
            if not has_meta and not has_votes:
                continue
            if not has_votes:
                sys.stderr.write(
                    f"  ! row {row_idx} {gemnr} {gname}: slot B{k:02d} has "
                    f"metadata ({titel!r}, {last!r}, {first!r}, {partei!r}) but "
                    f"no votes — dropping (likely source-CSV corruption)\n"
                )
                continue
            cands.append(dict(
                titel=titel, last=last, first=first, partei=partei,
                sti_hw=sti_hw, sti_sw=sti_sw,
            ))

        # Sanity — ANZBEW should equal the count of populated B-slots.
        # (0 ANZBEW happens when a Gemeinde row is nearly empty; the DSB
        #  gives no such rows in the current extract but we handle it.)
        if anzbew and len(cands) != anzbew:
            sys.stderr.write(
                f"  ! row {row_idx} {gemnr} {gname}: ANZBEW={anzbew} but "
                f"{len(cands)} candidate slots populated\n"
            )

        # Vote-integrity check (HW): sum of B{i}_STI == GUESTI
        sum_hw = sum((c["sti_hw"] or 0) for c in cands)
        if guesti_hw is not None and sum_hw > 0:
            if abs(sum_hw - guesti_hw) > 0.5:
                sys.stderr.write(
                    f"  ! row {row_idx} {gemnr} {gname}: HW sum {sum_hw} != "
                    f"GUESTI {guesti_hw}\n"
                )
                n_sum_mismatch_hw += 1

        # Vote-integrity check (SW): sum of B{i}_STI_SW == GUESTI_SW
        sum_sw = sum((c["sti_sw"] or 0) for c in cands)
        if guesti_sw is not None and sum_sw > 0:
            if abs(sum_sw - guesti_sw) > 0.5:
                sys.stderr.write(
                    f"  ! row {row_idx} {gemnr} {gname}: SW sum {sum_sw} != "
                    f"GUESTI_SW {guesti_sw}\n"
                )
                n_sum_mismatch_sw += 1

        # Identify the row-level winner among the candidates by (last, first).
        # StaLA's row header names the elected mayor explicitly, which is more
        # reliable than reverse-engineering the winner from rank (matters for
        # single-round elections where the elected candidate polled >50% AND
        # for uncontested single-candidate votes).
        winner_key = (winner_last, winner_first)
        is_win_by_slot = [
            (c["last"], c["first"]) == winner_key and winner_last != ""
            for c in cands
        ]
        # Sanity: exactly one match (or zero, if the winner slot was corrupted)
        if winner_last and sum(is_win_by_slot) != 1:
            sys.stderr.write(
                f"  ! row {row_idx} {gemnr} {gname}: winner "
                f"({winner_titel!r} {winner_last!r} {winner_first!r}) matches "
                f"{sum(is_win_by_slot)} candidate slots\n"
            )

        # Emit HW rows (one per candidate)
        turnout_hw = compute_turnout(waehler_hw, wber_hw)
        for c, is_win in zip(cands, is_win_by_slot):
            share = compute_share(c["sti_hw"], guesti_hw)
            out_rows.append(dict(
                ags=gemnr,
                ags_name=gname,
                state=STATE,
                state_name=STATE_NAME,
                election_year=election_year,
                election_date=hw_date,
                election_type=election_type,
                round="hauptwahl",
                eligible_voters=wber_hw,
                number_voters=waehler_hw,
                valid_votes=guesti_hw,
                invalid_votes=ungsti_hw,
                turnout=turnout_hw,
                candidate_name=(f"{c['last']}, {c['first']}").strip(", "),
                candidate_last_name=c["last"],
                candidate_first_name=c["first"],
                candidate_title=c["titel"],
                candidate_party=c["partei"],
                candidate_votes=c["sti_hw"],
                candidate_voteshare=share,
                # is_winner is TRUE on the SW row for elections with a runoff
                # (the SW is the decisive round); TRUE on the HW row otherwise.
                is_winner=is_win and sw_date is None,
                source_url=SOURCE_TAG,
            ))
        n_hw += 1

        # Emit SW rows (only for candidates who advanced, i.e. sti_sw is not None)
        if sw_date is not None and (waehler_sw is not None or any(c["sti_sw"] is not None for c in cands)):
            turnout_sw = compute_turnout(waehler_sw, wber_sw)
            emitted = 0
            for c, is_win in zip(cands, is_win_by_slot):
                if c["sti_sw"] is None:
                    continue
                share = compute_share(c["sti_sw"], guesti_sw)
                out_rows.append(dict(
                    ags=gemnr,
                    ags_name=gname,
                    state=STATE,
                    state_name=STATE_NAME,
                    election_year=int(sw_date[:4]),
                    election_date=sw_date,
                    election_type=election_type,
                    round="stichwahl",
                    eligible_voters=wber_sw,
                    number_voters=waehler_sw,
                    valid_votes=guesti_sw,
                    invalid_votes=ungsti_sw,
                    turnout=turnout_sw,
                    candidate_name=(f"{c['last']}, {c['first']}").strip(", "),
                    candidate_last_name=c["last"],
                    candidate_first_name=c["first"],
                    candidate_title=c["titel"],
                    candidate_party=c["partei"],
                    candidate_votes=c["sti_sw"],
                    candidate_voteshare=share,
                    is_winner=is_win,   # SW is the decisive round for runoff cycles
                    source_url=SOURCE_TAG,
                ))
                emitted += 1
            if emitted:
                n_sw += 1

    # Write output
    if not out_rows:
        sys.exit("no rows emitted — abort")

    cols = list(out_rows[0].keys())
    with open(OUT_CSV, "w", encoding="utf-8", newline="") as fh:
        w = csv.DictWriter(fh, fieldnames=cols)
        w.writeheader()
        for row in out_rows:
            # Normalise floats: convert to int when integral
            for k in ("eligible_voters", "number_voters", "valid_votes",
                      "invalid_votes", "candidate_votes"):
                v = row.get(k)
                if v is not None and v == int(v):
                    row[k] = int(v)
            w.writerow(row)

    print(f"wrote {OUT_CSV}")
    print(f"  {n_hw} Hauptwahl + {n_sw} Stichwahl elections")
    print(f"  {len(out_rows)} candidate rows total")
    for et, n in sorted(n_by_office.items()):
        print(f"  {et}: {n} elections")
    if n_sum_mismatch_hw or n_sum_mismatch_sw:
        print(f"  vote-integrity mismatches: {n_sum_mismatch_hw} HW, {n_sum_mismatch_sw} SW")
    else:
        print("  vote-integrity: OK (all HW & SW candidate sums match GÜLTIG)")


if __name__ == "__main__":
    main()
