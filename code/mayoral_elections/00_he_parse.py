#!/usr/bin/env python3
"""Stage-0 parser for Hessen (HE) mayoral (Bürgermeister/OB) + Landrat elections.

Source: Hessisches Statistisches Landesamt, Statistischer Bericht
"B VII m - Direktwahlen" — "Ergebnisse der jeweils letzten Direktwahlen der
Landrätinnen und Landräte sowie (Ober-)Bürgermeisterinnen und (Ober-)Bürgermeister
in Hessen" (Stand 06.05.2024), a landscape PDF (statistischebibliothek.de,
HEHeft_derivate_00013366). Like Baden-Württemberg's report it is a *snapshot*:
for each Gemeinde / Landkreis the most recent direct election (so Wahltage span
roughly 2017-2024). The 15 March 2026 Kommunalwahl is newer than this issue and
not yet included.

Each data row carries (landscape A4, fixed column bands):
  AGS | Landkreis/Gemeinde | [Stichwahl] | Wahltag | Wahlberechtigte |
  Wahlbeteiligung % | Wähler | Ungültige (Anzahl, %) | Gültige Stimmen |
  Gewählte/r Bewerber/in (Name, Träger des Wahlvorschlags=party, Geschlecht m/f) |
  Wahlvorschlag 1 (Bewerber/-in name, party, Anzahl, %)

Important source characteristics:
  * The published table shows only the NAMED WINNER + the FIRST Wahlvorschlag
    (the first candidate by ballot order). It does NOT list every candidate, and
    the winner's own vote count is shown only when the winner *is* Wahlvorschlag 1
    (otherwise NA). So this is essentially winner-level (with party + gender +
    full turnout) plus one extra candidate.
  * Elections with a Stichwahl produce TWO rows: the Stichwahl row (decisive,
    carries the named winner) and the Hauptwahl row (no winner named).
  * AGS: a 3-digit token = Landkreis (-> Landratswahl, AGS = 06+kkk+000);
    a "kkk ggg" pair = Gemeinde/kreisfreie Stadt (AGS = 06+kkkggg).

Office classifier (by AGS):
  * 3-digit AGS (Landkreis)                         -> Landratswahl
  * 5 kreisfreie Städte (6-digit ending "000")      -> Oberbürgermeisterwahl
  * 7 Sonderstatusstädte (pinned, OB by Hauptsatzung)-> Oberbürgermeisterwahl
  * everything else (Gemeinde)                      -> Bürgermeisterwahl
The mayoral Stage-1 split routes the Landratswahl rows to the landrat dataset.

Output (candidate-level long; one row per relevant candidate per round):
  data/mayoral_elections/raw/hessen/he_parsed.csv
Run:  python3 code/mayoral_elections/00_he_parse.py
"""

import csv
import os
import re
import sys

import pdfplumber

HERE = os.path.dirname(os.path.abspath(__file__))
ROOT = os.path.dirname(os.path.dirname(HERE))
RAW_DIR = os.path.join(ROOT, "data", "mayoral_elections", "raw", "hessen")
PDF = os.path.join(RAW_DIR, "HE_Direktwahlen_BVII-m05-2024_StatHessen.pdf")
# Output renamed to he_pdf_parsed.csv: this older 2024-PDF parse is now the
# FALLBACK source, merged under the richer 2026 XLSX by 00_he_parse_xlsx.py
# (which writes the final he_parsed.csv consumed by stages 01/01b).
OUT = os.path.join(RAW_DIR, "he_pdf_parsed.csv")

STATE, STATE_NAME = "06", "Hessen"

# Oberbürgermeister cities. 5 kreisfreie Städte are also caught by the
# "6-digit AGS ending 000" rule; the 7 Sonderstatusstädte are pinned by AGS.
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

# Column x0 bands (PDF points). Numbers are RIGHT-aligned, so a wide (6-digit)
# value's left edge shifts left of a narrow one's — the boundaries below are set
# for the widest case, and multi-token thousands groups ("508 510") are first
# merged into a single cell so each number occupies one band. The date/number
# boundary in particular must sit left of a 6-digit Wahlberechtigte's left edge.
BANDS = [
    ("ags", 0, 80), ("name", 80, 255), ("stichwahl", 255, 300), ("wahltag", 300, 349),
    ("wahlberechtigte", 349, 400), ("wahlbeteiligung", 400, 440), ("waehler", 440, 475),
    ("ungueltig_n", 475, 510), ("ungueltig_p", 510, 545), ("gueltig", 545, 585),
    ("win_name", 585, 685), ("win_party", 685, 760), ("win_gender", 760, 800),
    ("wv1_name", 800, 1008), ("wv1_party", 1008, 1078), ("wv1_n", 1078, 1125),
    ("wv1_p", 1125, 1300),
]
DATE_RE = re.compile(r"^\d{2}\.\d{2}\.\d{4}$")
NUM_TOK = re.compile(r"^\d{1,3}([.,]\d+)?$")  # a thousands group or %; NOT a date


def merge_numeric(words):
    """Merge adjacent space-separated number tokens (e.g. '508' '510' -> '508510')
    into one cell. Dates (two dots) are excluded by NUM_TOK so they never merge."""
    ws = sorted(words, key=lambda w: w["x0"])
    out = []
    for w in ws:
        if (out and NUM_TOK.match(w["text"]) and NUM_TOK.match(out[-1]["text"])
                and w["x0"] - out[-1]["x1"] < 14):
            out[-1] = {"text": out[-1]["text"] + w["text"],
                       "x0": out[-1]["x0"], "x1": w["x1"]}
        else:
            out.append({"text": w["text"], "x0": w["x0"], "x1": w["x1"]})
    return out


def band_of(x0):
    for name, lo, hi in BANDS:
        if lo <= x0 < hi:
            return name
    return None


def to_int(s):
    s = re.sub(r"\s", "", s or "")
    return int(s) if s.isdigit() else None


def to_share(s):
    s = re.sub(r"\s", "", s or "")
    if not s or s in (".", "-", "—"):
        return None
    s = s.replace(",", ".")
    try:
        return float(s) / 100.0
    except ValueError:
        return None


def norm_person(name):
    """Normalise a 'Last, First' name for winner<->Wahlvorschlag-1 comparison."""
    n = re.sub(r"^(?:(?:Prof\.|Dr\.|h\.\s*c\.)\s*)+", "", name or "")
    return re.sub(r"\s+", " ", n).strip().lower()


def split_name(full):
    full = re.sub(r"\s+", " ", full or "").strip()
    if "," in full:
        last, first = [x.strip() for x in full.split(",", 1)]
    else:
        last, first = full, ""
    last = re.sub(r"^(?:(?:Prof\.|Dr\.|h\.\s*c\.)\s*)+", "", last).strip()
    return full, (last or full), first


def iso(de):
    m = re.match(r"(\d{2})\.(\d{2})\.(\d{4})", de or "")
    return f"{m.group(3)}-{m.group(2)}-{m.group(1)}" if m else ""


def cluster_rows(words):
    rows = []
    for w in sorted(words, key=lambda x: (x["top"], x["x0"])):
        if rows and abs(w["top"] - rows[-1][0]) <= 4:
            rows[-1][1].append(w)
        else:
            rows.append([w["top"], [w]])
    return [r[1] for r in rows]


def cell_text(words, joiner=" "):
    """Join words (sorted by x0); collapse spaces. For numbers pass joiner=''."""
    return joiner.join(w["text"] for w in sorted(words, key=lambda x: x["x0"])).strip()


def parse_row(words):
    """Return a dict for a data row, or None if it's not a data row."""
    cols = {name: [] for name, _, _ in BANDS}
    for w in merge_numeric(words):
        b = band_of(w["x0"])
        if b:
            cols[b].append(w)
    ags_cell = cell_text(cols["ags"], "")            # merged AGS (e.g. "412000" or "431")
    wahltag = cell_text(cols["wahltag"], "")
    # data row = an all-digit AGS cell (3-digit Kreis or 6-digit Gemeinde) + a Wahltag
    if not DATE_RE.match(wahltag) or not ags_cell.isdigit():
        return None
    if len(ags_cell) == 6:
        ags = STATE + ags_cell                       # Gemeinde / kreisfreie Stadt
        is_kreis = False
    elif len(ags_cell) == 3:
        ags = STATE + ags_cell + "000"               # Landkreis
        is_kreis = True
    else:
        return None

    if is_kreis:
        etype = "Landratswahl"
    elif ags in OB_ALL or ags[2:].endswith("000"):
        etype = "Oberbürgermeisterwahl"
    else:
        etype = "Bürgermeisterwahl"

    return {
        "ags": ags,
        "ags_name": cell_text(cols["name"], " "),
        "election_type": etype,
        "round": "stichwahl" if "Stichwahl" in cell_text(cols["stichwahl"], " ") else "hauptwahl",
        "date": iso(wahltag),
        "eligible": to_int(cell_text(cols["wahlberechtigte"], "")),
        "voters": to_int(cell_text(cols["waehler"], "")),
        "invalid": to_int(cell_text(cols["ungueltig_n"], "")),
        "valid": to_int(cell_text(cols["gueltig"], "")),
        "turnout_pct": to_share(cell_text(cols["wahlbeteiligung"], "")),
        "win_name": cell_text(cols["win_name"], " "),
        "win_party": cell_text(cols["win_party"], " "),
        "win_gender": cell_text(cols["win_gender"], " "),
        "wv1_name": cell_text(cols["wv1_name"], " "),
        "wv1_party": cell_text(cols["wv1_party"], " "),
        "wv1_votes": to_int(cell_text(cols["wv1_n"], "")),
        "wv1_share": to_share(cell_text(cols["wv1_p"], "")),
    }


FIELDS = ["ags", "ags_name", "state", "state_name", "election_year", "election_date",
          "election_type", "round", "eligible_voters", "number_voters", "valid_votes",
          "invalid_votes", "turnout", "candidate_name", "candidate_last_name",
          "candidate_first_name", "candidate_gender", "candidate_party",
          "candidate_votes", "candidate_voteshare", "is_winner", "candidate_rank",
          "source_file"]


def emit(r):
    """Yield candidate-level output rows for one parsed data row."""
    ev, nv = r["eligible"], r["voters"]
    turnout = r["turnout_pct"] if r["turnout_pct"] is not None else (
        (nv / ev) if (ev and nv) else "")
    base = {
        "ags": r["ags"], "ags_name": r["ags_name"], "state": STATE, "state_name": STATE_NAME,
        "election_year": r["date"][:4], "election_date": r["date"],
        "election_type": r["election_type"], "round": r["round"],
        "eligible_voters": ev if ev is not None else "",
        "number_voters": nv if nv is not None else "",
        "valid_votes": r["valid"] if r["valid"] is not None else "",
        "invalid_votes": r["invalid"] if r["invalid"] is not None else "",
        "turnout": turnout, "source_file": os.path.basename(PDF),
    }
    win = r["win_name"]
    wv1 = r["wv1_name"]
    gender = {"m": "male", "f": "female"}.get(r["win_gender"].strip().lower(), "")

    def gender_to(g):
        return {"m": "male", "f": "female"}.get(g, "")

    out = []
    if win:                                          # decisive round: winner named
        same = wv1 and norm_person(win) == norm_person(wv1)
        full, last, first = split_name(win)
        out.append({**base, "candidate_name": full, "candidate_last_name": last,
                    "candidate_first_name": first, "candidate_gender": gender,
                    "candidate_party": r["win_party"],
                    "candidate_votes": r["wv1_votes"] if same else "",
                    "candidate_voteshare": r["wv1_share"] if same else "",
                    "is_winner": "TRUE", "candidate_rank": 1})
        if wv1 and not same:                         # Wahlvorschlag 1 = a losing candidate
            full2, last2, first2 = split_name(wv1)
            out.append({**base, "candidate_name": full2, "candidate_last_name": last2,
                        "candidate_first_name": first2, "candidate_gender": "",
                        "candidate_party": r["wv1_party"],
                        "candidate_votes": r["wv1_votes"] if r["wv1_votes"] is not None else "",
                        "candidate_voteshare": r["wv1_share"] if r["wv1_share"] is not None else "",
                        "is_winner": "FALSE", "candidate_rank": ""})
    elif wv1:                                        # non-decisive Hauptwahl row: only WV1
        full2, last2, first2 = split_name(wv1)
        out.append({**base, "candidate_name": full2, "candidate_last_name": last2,
                    "candidate_first_name": first2, "candidate_gender": "",
                    "candidate_party": r["wv1_party"],
                    "candidate_votes": r["wv1_votes"] if r["wv1_votes"] is not None else "",
                    "candidate_voteshare": r["wv1_share"] if r["wv1_share"] is not None else "",
                    "is_winner": "FALSE", "candidate_rank": ""})
    return out


def main():
    if not os.path.exists(PDF):
        raise SystemExit(f"Raw PDF not found: {PDF}")
    parsed = []
    with pdfplumber.open(PDF) as pdf:
        for pg in pdf.pages:
            words = [w for w in pg.extract_words(use_text_flow=False, keep_blank_chars=False)
                     if w["top"] > 160]   # skip page title/header band
            for row_words in cluster_rows(words):
                r = parse_row(row_words)
                if r:
                    parsed.append(r)

    rows = []
    for r in parsed:
        rows.extend(emit(r))

    # diagnostics
    munis = {r["ags"] for r in parsed}
    by_type = {}
    for r in parsed:
        by_type[r["election_type"]] = by_type.get(r["election_type"], 0) + 1
    ob_found = {r["ags"] for r in parsed if r["election_type"] == "Oberbürgermeisterwahl"}
    n_win = sum(1 for r in parsed if r["win_name"])
    sys.stderr.write(
        f"\n=== Hessen Direktwahlen parse ===\n"
        f"  data rows:        {len(parsed)}\n"
        f"  distinct AGS:     {len(munis)}\n"
        f"  with named winner:{n_win}\n"
        f"  by election_type: {by_type}\n"
        f"  OB AGS found:     {len(ob_found)} (expected {len(OB_ALL)})\n"
        f"  candidate rows:   {len(rows)}\n")
    missing_ob = OB_ALL - ob_found
    if missing_ob:
        raise SystemExit(f"FATAL: pinned OB cities missing from parse: {sorted(missing_ob)}")

    with open(OUT, "w", newline="", encoding="utf-8") as fh:
        w = csv.DictWriter(fh, fieldnames=FIELDS)
        w.writeheader()
        w.writerows(rows)
    sys.stderr.write(f"  Wrote {len(rows)} rows -> {OUT}\n")


if __name__ == "__main__":
    main()
