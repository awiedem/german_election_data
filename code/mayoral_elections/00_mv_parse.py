#!/usr/bin/env python3
"""
Stage 0 — Mecklenburg-Vorpommern Bürgermeister/Landrat direct elections.

Parses the 69 LAIV-MV "Direktwahlen" result PDFs in
  data/mayoral_elections/raw/mecklenburg_vorpommern/
into a single LONG candidate-level intermediate
  data/mayoral_elections/raw/mecklenburg_vorpommern/mv_parsed.csv
(one row per candidate per round) that the R Stage-1 scripts
(01_mayoral_unharm.R, 01b_mayoral_candidates.R) ingest via an SH-style block.

Why Python + coordinate-based parsing (pdfplumber word x-positions) instead of
text heuristics: the MV PDFs have multi-line party cells (a coalition wraps over
2-3 lines), three different thousand-separator conventions (space / dot / none),
Ja/Nein single-candidate runoffs, and Neuwahl re-runs. Assigning each word to a
column by its x-coordinate is far more robust than splitting whitespace.

The source PDFs carry NO AGS, so AGS is assigned from a verified name->code map
(see AGS section). The map is year-aware because the 2011 Kreisgebietsreform
renumbered the Landkreise and merged four kreisfreie Städte into Landkreise.

Run:  python3 code/mayoral_elections/00_mv_parse.py
"""

import os
import re
import sys
import csv
import glob
import unicodedata

import pdfplumber

HERE = os.path.dirname(os.path.abspath(__file__))
ROOT = os.path.abspath(os.path.join(HERE, "..", ".."))
RAW = os.path.join(ROOT, "data", "mayoral_elections", "raw", "mecklenburg_vorpommern")
OUT_CSV = os.path.join(RAW, "mv_parsed.csv")
OUT_ELEC = os.path.join(RAW, "mv_parsed_elections.csv")

STATE = "13"
STATE_NAME = "Mecklenburg-Vorpommern"

MONTHS = {
    "Januar": 1, "Februar": 2, "März": 3, "April": 4, "Mai": 5, "Juni": 6,
    "Juli": 7, "August": 8, "September": 9, "Oktober": 10, "November": 11,
    "Dezember": 12,
}

# ---------------------------------------------------------------------------
# AGS map (verified against data/crosswalks/final/cty_crosswalks.csv,
# county_elec_unharm.rds, and ags_crosswalks.csv — see session notes).
# Keys are the canonical unit names as they appear in the PDF titles.
# ---------------------------------------------------------------------------
# Landkreis 5-digit county code (Landrat AGS = code5 + "000"); year-aware.
KREIS_PRE = {  # elections <= 2010 (pre-reform)
    "Bad Doberan": "13051", "Demmin": "13052", "Güstrow": "13053",
    "Ludwigslust": "13054", "Mecklenburg-Strelitz": "13055", "Müritz": "13056",
    "Nordvorpommern": "13057", "Nordwestmecklenburg": "13058",
    "Ostvorpommern": "13059", "Parchim": "13060", "Rügen": "13061",
    "Uecker-Randow": "13062",
}
KREIS_POST = {  # elections >= 2011 (post-reform)
    "Mecklenburgische Seenplatte": "13071", "Rostock": "13072",
    "Vorpommern-Rügen": "13073", "Nordwestmecklenburg": "13074",
    "Vorpommern-Greifswald": "13075", "Ludwigslust-Parchim": "13076",
}
# kreisfreie / große Städte: full 8-digit municipality AGS; year-aware.
CITY_PRE = {  # <= 2010
    "Greifswald": "13001000", "Neubrandenburg": "13002000", "Rostock": "13003000",
    "Schwerin": "13004000", "Stralsund": "13005000", "Wismar": "13006000",
}
CITY_POST = {  # >= 2011 (Rostock/Schwerin stayed kreisfrei; other four merged)
    "Greifswald": "13075039", "Neubrandenburg": "13071107", "Rostock": "13003000",
    "Schwerin": "13004000", "Stralsund": "13073088", "Wismar": "13074087",
}


def norm_unit(name):
    """Canonicalise a unit name for map lookup."""
    n = name.strip()
    n = re.sub(r"^Landkreis\s+", "", n)        # "Landkreis Rostock" -> "Rostock"
    n = re.sub(r"^(Hanse(-?\s*und\s*Universitäts)?stadt|Hansestadt|Universitäts-?\s*und\s*Hansestadt)\s+", "", n)
    n = n.replace("­", "").strip()        # soft hyphen
    n = re.sub(r"\s+", " ", n)
    return n


def assign_ags(office, unit, year):
    """Return (ags, ags_name) for a (office, unit, year). Raises on ambiguity."""
    u = norm_unit(unit)
    if office == "Landratswahl":
        table = KREIS_POST if year >= 2011 else KREIS_PRE
        if u not in table:
            raise ValueError(f"Landkreis '{u}' (from '{unit}') not in {year} Kreis map")
        return table[u] + "000", u
    else:  # Oberbürgermeisterwahl / Bürgermeisterwahl
        table = CITY_POST if year >= 2011 else CITY_PRE
        if u not in table:
            raise ValueError(f"Stadt '{u}' (from '{unit}') not in {year} city map")
        return table[u], u


# ---------------------------------------------------------------------------
# Low-level helpers
# ---------------------------------------------------------------------------
def parse_int(tokens):
    """Join digit tokens (space/dot thousand separators) into an int, or None."""
    if not tokens:
        return None
    s = "".join(tokens)
    s = s.replace(".", "").replace(" ", "").replace(" ", "")
    s = re.sub(r"[^\d]", "", s)
    return int(s) if s else None


def parse_pct(tok):
    """'28,7' -> 0.287 ; None on failure."""
    if tok is None:
        return None
    s = tok.replace("%", "").replace(" ", "").replace(" ", "").strip()
    s = s.replace(".", "").replace(",", ".") if s.count(",") else s.replace(",", ".")
    try:
        return float(s) / 100.0
    except ValueError:
        return None


def parse_german_date(text, year):
    """Find a 'DD. Monat YYYY' date in text; return ISO string or None."""
    m = re.search(r"(\d{1,2})\.\s*([A-Za-zÄÖÜäöü]+)\s*(\d{4})", text)
    if not m:
        return None
    d, mon, y = int(m.group(1)), m.group(2), int(m.group(3))
    if mon not in MONTHS:
        return None
    return f"{y:04d}-{MONTHS[mon]:02d}-{d:02d}"


def is_num_token(t):
    return bool(re.fullmatch(r"[\d. ]+", t.replace(" ", "")))


# ---------------------------------------------------------------------------
# Word / line extraction
# ---------------------------------------------------------------------------
def get_lines(page):
    """Return list of lines; each line = list of word dicts sorted by x0.
    A line groups words with similar 'top' (y) coordinate."""
    words = page.extract_words(use_text_flow=False, keep_blank_chars=False,
                               x_tolerance=1.5, y_tolerance=3)
    words.sort(key=lambda w: (round(w["top"]), w["x0"]))
    lines = []
    cur, cur_top = [], None
    for w in words:
        if cur_top is None or abs(w["top"] - cur_top) <= 3:
            cur.append(w)
            cur_top = w["top"] if cur_top is None else cur_top
        else:
            lines.append(sorted(cur, key=lambda x: x["x0"]))
            cur, cur_top = [w], w["top"]
    if cur:
        lines.append(sorted(cur, key=lambda x: x["x0"]))
    return lines


def line_text(line):
    return " ".join(w["text"] for w in line)


def cluster(line, gap=22):
    """Group a line's words into x-clusters (new group when x-gap > `gap` px)."""
    groups, cur = [], []
    for w in sorted(line, key=lambda x: x["x0"]):
        if cur and w["x0"] - cur[-1]["x1"] > gap:
            groups.append(cur); cur = []
        cur.append(w)
    if cur:
        groups.append(cur)
    return groups


def gtext(g):
    return " ".join(w["text"] for w in g)


# ---------------------------------------------------------------------------
# Core per-PDF parser
# ---------------------------------------------------------------------------
def parse_pdf(path, fname):
    notes = []
    with pdfplumber.open(path) as pdf:
        page = pdf.pages[0]
        lines = get_lines(page)
        full_text = page.extract_text() or ""

    # ---- title / office / unit / year ----
    # Title is the first 1-2 non-empty lines, truncated before the date line.
    title = " ".join(line_text(l) for l in lines[:2])
    title = re.sub(r"\s+", " ", title).strip()
    title_head = re.split(r"\bTag der\b", title)[0].strip()

    low = title_head.lower()
    if "landrat" in low or "landrät" in low or "landrates" in low:
        office = "Landratswahl"
    elif "oberbürgermeister" in low:
        office = "Oberbürgermeisterwahl"
    elif "bürgermeister" in low:
        office = "Bürgermeisterwahl"
    else:
        raise ValueError(f"{fname}: cannot determine office from title: {title!r}")

    ym = re.search(r"(19|20)\d{2}", title_head) or re.search(r"(19|20)\d{2}", fname)
    year = int(ym.group(0))

    # unit name: trailing token(s) before the optional year.
    #  - Landrat: after "im Landkreis"
    #  - City: after the last "...stadt"/"Stadt" descriptor (varies widely:
    #    "kreisfreien Stadt", "Hansestadt", "Universitäts- und Hansestadt",
    #    "Landeshauptstadt", "kreisfreien Hanse- und Universitätsstadt", ...)
    unit = None
    if office == "Landratswahl":
        m = re.search(r"im Landkreis\s+(.+?)(?:\s+(?:19|20)\d{2})?\s*$", title_head)
        if m:
            unit = m.group(1).strip()
    else:
        m = re.search(r".*(?:Stadt|stadt)\s+(.+?)(?:\s+(?:19|20)\d{2})?\s*$", title_head)
        if m:
            unit = m.group(1).strip()
    if unit is None:
        # fall back to filename token (e.g. "KW D 2001 Parchim" / "2018_LRW_Landkreis X")
        base = os.path.splitext(fname)[0]
        base = re.sub(r"^(KW D \d{4}|2\d{3}_(BgmWahl|LRW))[_ ]+", "", base)
        base = re.sub(r"^Landkreis[- ]+", "", base)
        base = re.sub(r"^\d{2}\s+", "", base)  # "2001 05 Rügen" leftover
        base = re.sub(r",.*$", "", base)       # "Stralsund, Hansestadt" -> "Stralsund"
        unit = base.replace("-", " ").strip()
        notes.append("unit_from_filename")

    ags, ags_name = assign_ags(office, unit, year)

    # ---- dates ----
    hw_date = sw_date = None
    for l in lines:
        t = line_text(l)
        if re.search(r"Tag der (Hauptwahl|Neuwahl)", t):
            hw_date = parse_german_date(t, year)
        elif re.search(r"Tag der Stichwahl", t):
            if not re.search(r"entfällt", t, re.I):
                sw_date = parse_german_date(t, year)

    # ---- locate the candidate-table header (the line containing 'Partei') ----
    hdr_idx = None
    for i, l in enumerate(lines):
        if any(w["text"].startswith("Partei") for w in l):
            hdr_idx = i
            break
    if hdr_idx is None:
        raise ValueError(f"{fname}: no 'Partei' header found")

    # ---- Merkmal block (turnout aggregates) — two columns HW / SW ----
    # For each Merkmal row, cluster its words by x-gap; the numeric groups are
    # the HW value (first) and SW value (second). This is robust to the three
    # thousand-separator conventions and to header/column misalignment.
    def agg(label_re):
        for l in lines[:hdr_idx + 1]:
            if re.search(label_re, line_text(l)):
                groups = cluster(l)
                num_vals = []
                for g in groups:
                    gt = gtext(g).strip()
                    gtc = gt.replace(" ", "").replace(".", "")
                    if re.fullmatch(r"\d+", gtc):
                        num_vals.append(int(gtc))
                    elif gt == "-":
                        num_vals.append(None)
                hw = num_vals[0] if len(num_vals) >= 1 else None
                sw = num_vals[1] if len(num_vals) >= 2 else None
                return hw, sw
        return None, None

    # Anchored labels: "gültige Stimmen" is a substring of "ungültige Stimmen",
    # so valid must exclude the un- prefix.
    eligible_hw, eligible_sw = agg(r"Wahlberechtigte")
    voters_hw, voters_sw = agg(r"(?:^|\s)(?:Wähler\b|Wählerinnen)")
    invalid_hw, invalid_sw = agg(r"[Uu]ngültige Stimmen")
    valid_hw, valid_sw = agg(r"(?:^|\s)[Gg]ültige Stimmen")

    aggs = {
        "hauptwahl": dict(eligible=eligible_hw, voters=voters_hw,
                          valid=valid_hw, invalid=invalid_hw),
        "stichwahl": dict(eligible=eligible_sw, voters=voters_sw,
                          valid=valid_sw, invalid=invalid_sw),
    }

    # ---- candidate rows (x-gap clustering) ----
    # Determine the name-column left margin from the 'Hauptwahl' section label.
    name_margin = None
    for l in lines[hdr_idx + 1:]:
        if line_text(l).strip() == "Hauptwahl":
            name_margin = l[0]["x0"]
            break
    if name_margin is None:
        name_margin = min((w["x0"] for l in lines[hdr_idx + 1:] for w in l), default=70)

    SHARE_RE = re.compile(r"^\d{1,3}(?:[.\s]\d{3})*,\d+\s*%?$")
    INT_RE = re.compile(r"^\d{1,3}(?:[.\s]\d{3})*$|^\d+$")

    def classify(gtxts):
        out = []
        for gt in gtxts:
            gtc = gt.replace(" ", "")
            if SHARE_RE.match(gt):
                out.append("share")
            elif INT_RE.match(gtc):
                out.append("int")
            else:
                out.append("text")
        return out

    # A candidate's cells can wrap over 2-3 lines (long coalition parties), and
    # the votes may sit on the name line OR on a party-continuation line. We
    # therefore accumulate: `pending` holds a candidate whose votes are not yet
    # seen; `last` is the last completed candidate (to absorb trailing party
    # lines). Gap threshold 13px separates name|party (min observed gap 16px)
    # without splitting name-internal or thousand-separator gaps (<=5px).
    cands = []
    pending = None
    last = None
    cur_round = None
    ja_nein = False

    for l in lines[hdr_idx + 1:]:
        t = line_text(l).strip()
        if t == "Hauptwahl":
            cur_round = "hauptwahl"; ja_nein = False; pending = last = None; continue
        if t == "Stichwahl":
            cur_round = "stichwahl"; ja_nein = False; pending = last = None; continue
        if t.startswith("Wahlsieger"):
            break
        if re.match(r"^\d+\)\s", t) or t.startswith("______") or t.startswith("("):
            continue
        if cur_round is None:
            continue
        if "Ja" in t and "Nein" in t and not any(is_num_token(w["text"]) for w in l):
            ja_nein = True
            notes.append("ja_nein_stichwahl")
            continue

        groups = cluster(l, gap=13)
        gtxts = [gtext(g).strip() for g in groups]
        types = classify(gtxts)
        int_idx = [i for i, ty in enumerate(types) if ty == "int"]
        share_idx = [i for i, ty in enumerate(types) if ty == "share"]
        text_idx = [i for i, ty in enumerate(types) if ty == "text"]
        has_votes = bool(int_idx) or bool(share_idx)

        name_present = bool(text_idx) and groups[text_idx[0]][0]["x0"] <= name_margin + 30
        party_here = [gtxts[i] for i in text_idx if not (name_present and i == text_idx[0])]

        def extract_votes_share():
            share = parse_pct(gtxts[share_idx[-1]]) if share_idx else None
            if ja_nein:                                   # "Ja" column only
                votes = parse_int([gtxts[int_idx[0]]]) if int_idx else None
            else:
                v = [i for i in int_idx if not share_idx or i < share_idx[-1]]
                votes = parse_int([gtxts[v[-1]]]) if v else None
            return votes, share

        if name_present:
            if pending is not None:                       # flush dangling pending
                cands.append(pending); last = pending; pending = None
            cand = dict(round=cur_round, name=gtxts[text_idx[0]],
                        party_parts=list(party_here), votes=None, share=None,
                        ja_nein=ja_nein)
            if has_votes:
                cand["votes"], cand["share"] = extract_votes_share()
                cands.append(cand); last = cand
            else:
                pending = cand                            # await the votes line
        elif has_votes:
            # votes line completing a pending candidate (party wrapped above)
            if pending is not None:
                pending["party_parts"].extend(party_here)
                pending["votes"], pending["share"] = extract_votes_share()
                cands.append(pending); last = pending; pending = None
        elif party_here:
            # pure party-continuation line
            target = pending if pending is not None else last
            if target is not None:
                target["party_parts"].extend(party_here)
    if pending is not None:
        cands.append(pending)

    # ---- winner ----
    wm = re.search(r"Wahlsieger(?:in)?:\s*(.+)", full_text)
    winner_raw = None
    if wm:
        winner_raw = wm.group(1).strip()
        winner_raw = re.split(r"\n", winner_raw)[0].strip()
        if winner_raw.lower().startswith("neuwahl") or "nicht erreicht" in winner_raw.lower():
            notes.append("no_winner_neuwahl")
            winner_raw = None

    # ---- build long rows ----
    rows = []
    for c in cands:
        party = " ".join(p for p in c["party_parts"] if p)
        party = re.sub(r"\s+", " ", party)
        party = re.sub(r"\s*,\s*", ", ", party)          # normalise comma spacing
        party = re.sub(r"(?:,\s*)+", ", ", party)         # collapse repeated commas
        party = party.strip().strip(",").strip()
        # Drop trailing footnote markers WITHOUT clobbering digits that belong to
        # the party name (e.g. "B90"). Two safe forms: " 2)" and a lone digit that
        # directly follows a letter ("ask Schwerin1" -> "ask Schwerin"; "B90" kept).
        party = re.sub(r"\s*\d+\)\s*$", "", party)
        party = re.sub(r"(?<=[A-Za-zÄÖÜäöüß])\d\s*$", "", party).strip()
        name = c["name"].strip()
        # split last, first on first comma
        if "," in name:
            last, first = name.split(",", 1)
            last, first = last.strip(), first.strip()
        else:
            last, first = name, ""
        a = aggs[c["round"]]
        turnout = (a["voters"] / a["eligible"]) if (a["voters"] and a["eligible"]) else None
        rows.append(dict(
            ags=ags, ags_name=ags_name, state=STATE, state_name=STATE_NAME,
            election_year=year, election_date=(hw_date if c["round"] == "hauptwahl" else sw_date),
            election_type=office, round=c["round"],
            eligible_voters=a["eligible"], number_voters=a["voters"],
            valid_votes=a["valid"], invalid_votes=a["invalid"], turnout=turnout,
            candidate_name=name, candidate_last_name=last, candidate_first_name=first,
            candidate_party=party, candidate_votes=c["votes"], candidate_voteshare=c["share"],
            ja_nein=int(c["ja_nein"]),
            winner_name_raw=winner_raw or "",
            source_file=fname, notes=";".join(sorted(set(notes))),
        ))
    elec = dict(ags=ags, ags_name=ags_name, office=office, year=year,
                hw_date=hw_date, sw_date=sw_date or "", n_cands=len(rows),
                source_file=fname, notes=";".join(sorted(set(notes))))
    return rows, elec


def main():
    pdfs = sorted(glob.glob(os.path.join(RAW, "*.pdf")))
    if not pdfs:
        sys.exit(f"No PDFs found in {RAW}")
    all_rows, all_elec, errors = [], [], []
    for p in pdfs:
        fname = os.path.basename(p)
        try:
            rows, elec = parse_pdf(p, fname)
            all_rows.extend(rows)
            all_elec.append(elec)
        except Exception as e:
            errors.append((fname, str(e)))
            print(f"  ERROR {fname}: {e}", file=sys.stderr)

    cols = ["ags", "ags_name", "state", "state_name", "election_year",
            "election_date", "election_type", "round", "eligible_voters",
            "number_voters", "valid_votes", "invalid_votes", "turnout",
            "candidate_name", "candidate_last_name", "candidate_first_name",
            "candidate_party", "candidate_votes", "candidate_voteshare",
            "ja_nein", "winner_name_raw", "source_file", "notes"]
    with open(OUT_CSV, "w", newline="", encoding="utf-8") as fh:
        w = csv.DictWriter(fh, fieldnames=cols)
        w.writeheader()
        for r in all_rows:
            w.writerow(r)
    ecols = ["ags", "ags_name", "office", "year", "hw_date", "sw_date",
             "n_cands", "source_file", "notes"]
    with open(OUT_ELEC, "w", newline="", encoding="utf-8") as fh:
        w = csv.DictWriter(fh, fieldnames=ecols)
        w.writeheader()
        for e in all_elec:
            w.writerow(e)

    print(f"Parsed {len(all_elec)}/{len(pdfs)} PDFs, {len(all_rows)} candidate rows")
    print(f"  -> {OUT_CSV}")
    print(f"  -> {OUT_ELEC}")
    if errors:
        print(f"\n{len(errors)} ERRORS:")
        for f, e in errors:
            print(f"  {f}: {e}")
        sys.exit(1)


if __name__ == "__main__":
    main()
