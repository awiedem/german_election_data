#!/usr/bin/env python3
"""Stage-0 parser for NIEDERSACHSEN Landtagswahl 2008 results at WAHLKREIS level.

Source (raw, read-only):
  data/state_elections/raw/Landtagswahlen_Wahlkreis/Niedersachsen/
    NI_2008_Landtagswahl_StatBericht.pdf
  = Landesamt fuer Statistik Niedersachsen, Statistischer Bericht B VII 2-2 - 2j/08,
    "Die Wahl zum Niedersaechsischen Landtag am 27. Januar 2008" (378 pp).

Why a PDF parser: parse_NI.R already covers 1998/2003 (HTML) and 2013/2017/2022
(CSV+XML) from the LSN WahlServer, but that portal only goes back to 2013. 2008
has no machine-readable open-data file; this Statistischer Bericht is the only
digitised source, and it carries a TEXT LAYER (no OCR needed).

  Table 3 (PDF pp. 57-100, 0-indexed) "Ergebnisse der Landtagswahl 2008 - Erst-
  und Zweitstimmen - nach Landtagswahlkreisen mit Vergleichszahlen der
  Landtagswahl 2003 [...], der Bundestagswahl 2005 [...] und der Europawahl
  2004" reports, per Wahlkreis, FOUR comparison sub-blocks: L 08 (this
  election), L 03, B 05, E 04. We keep ONLY "L 08". NI's 2008 Wahlkreis
  boundaries differ from 2013+, so this uses the report's own WK numbers/names
  with no cross-year identity check (flagged in the README, not here).

  Layout: one physical table split across alternating page halves.
    LEFT  page: Wahlberechtigte/Waehler/ungueltige/gueltige + CDU/SPD/FDP/
                GRUENE/DIE LINKE. Niedersachsen (9 cols); carries the WK/Land
                block headers ("<nr> <name>" / "Land Niedersachsen") and the
                block markers ("L 08"/"L 03"/"B 05"/"E 04") + row labels
                ("Zahl I"/"%"/"Zahl II"/"%", or "Zahl"/"%" for E04's single
                Europawahl vote).
    RIGHT page: the remaining 13 minor-party columns (Volksabstimmung, Die
                Weissen, Die Friesen, GRAUE, REP, FAMILIE, FW, Die
                Tierschutzpartei, NPD, oedp, PBC, EB, Sonstige) - UNLABELLED,
                same row order as the left page, no WK headers at all.
  Each left page is immediately followed by its right-half continuation page
  (pairs run 57+58, 59+60, ..., 99+100); every pair covers the same 4 units
  (WK or the trailing Land row) in identical row order, so pairing is done by
  positional zip within each page pair, with a hard length check that aborts
  if the two halves ever desynchronise.

PARSING METHOD (coordinate based). German thousands separator is a space, so
"6 087 297" arrives as 3 word tokens; digit-groups are re-merged (a token that
is all-digit followed by an exactly-3-digit token, gap < 6pt). Column IDENTITY
is pinned per column via its right edge x1 (numbers are right-aligned; x0
drifts left as a value gets narrower, x1 does not) - LEFT_ANCHORS/RIGHT_ANCHORS
below were read directly off the PDF (WK 1 for the left table, WK 1 + the Land
row for the right table) and every classified cell must land within
EDGE_TOL of its anchor or the run aborts.

"-" = no candidate on the ballot -> NA (not zero).

VALIDATION (all hard; nothing is written if any check fails):
  (1) exactly 87 Wahlkreise + the Land row, both Erststimme and Zweitstimme
  (2) per (WK, stimme): sum of ALL 18 party columns (5 left + 13 right)
      == gueltige Stimmen, exactly - this can only hold once the right-page
      minor parties are correctly zipped in
  (3) per party, both stimme: sum over the 87 Wahlkreise == the report's own
      printed Land Niedersachsen row, exactly
  (4) per (WK, stimme): Waehler == gueltige + ungueltige
  (5) pinned official Zweitstimme shares (Statistisches Bundesamt / LSN final
      result), +-0.1pp: CDU 42.5, SPD 30.3, FDP 8.2, DIE LINKE 7.1, GRUENE 8.0

Output: data/state_elections/processed/wahlkreis/ni_pdf/NI_2008_pdf_long.csv
        (read by parsers/parse_NI.R, which appends it to the 1998-2022 series)
Run:    python3 code/state_elections_wahlkreis/parsers/00_ni_pdf_parse.py
Requires: pdfplumber.
"""

import csv
import os
import re
import sys

import pdfplumber

HERE = os.path.dirname(os.path.abspath(__file__))
ROOT = os.path.dirname(os.path.dirname(os.path.dirname(HERE)))
PDF = os.path.join(
    ROOT, "data", "state_elections", "raw", "Landtagswahlen_Wahlkreis", "Niedersachsen",
    "NI_2008_Landtagswahl_StatBericht.pdf")
OUT_DIR = os.path.join(ROOT, "data", "state_elections", "processed", "wahlkreis", "ni_pdf")
OUT = os.path.join(OUT_DIR, "NI_2008_pdf_long.csv")

STATE_ABBR, STATE_NAME = "NI", "Niedersachsen"
ELECTION_YEAR = 2008
ELECTION_DATE = "2008-01-27"
N_WKR = 87

# Table 3, 0-indexed pdfplumber page numbers (57 = first left page, 100 = last
# right page, the one carrying the Land Niedersachsen row). Verified by
# scanning for the table-3 header text and the Table-4 ("Bundestagswahlkreis")
# header that follows immediately after page 100.
PAGE_START, PAGE_END = 57, 100

# --- pinned column anchors (x1 = right edge, pt) -----------------------------
# LEFT page: read off WK 1 (Braunschweig-Nord), L08 Zahl I row.
LEFT_COLS = ["eligible_voters", "number_voters", "invalid_votes", "valid_votes",
             "CDU", "SPD", "FDP", "GRÜNE", "DIE LINKE. Niedersachsen"]
LEFT_ANCHORS = [208.7, 253.8, 288.4, 336.3, 381.4, 426.3, 465.7, 504.7, 548.3]

# RIGHT page: read off WK 1, L08 Zahl II row (the Volksabstimmung..Sonstige
# minor-party columns; header text reconstructed from the 3-line wrapped
# column headers, e.g. "Tier-"/"schutz-"/"partei" -> "Die Tierschutzpartei").
RIGHT_COLS = ["Volksabstimmung", "Die Weissen", "Die Friesen", "GRAUE", "REP",
              "FAMILIE", "FW", "Die Tierschutzpartei", "NPD", "ödp", "PBC",
              "EB", "Sonstige"]
RIGHT_ANCHORS = [84.5, 124.6, 164.8, 202.5, 242.6, 282.8, 321.3, 361.5, 399.2,
                  437.8, 474.8, 511.0, 546.4]

ALL_PARTY_COLS = LEFT_COLS[4:] + RIGHT_COLS  # the 5 + 13 party columns

EDGE_TOL = 3.5     # pt; digit-width variation on right-aligned cells
MERGE_GAP = 6.0     # pt; intra-number gaps are ~2pt, inter-column gaps are much larger
DASH = {"-", "–", "—"}

# official statewide Zweitstimme shares (Landeswahlleiterin Niedersachsen /
# Destatis final result), pinned to +-0.1pp
OFFICIAL_ZWEITSTIMME = {"CDU": 42.5, "SPD": 30.3, "FDP": 8.2,
                        "DIE LINKE. Niedersachsen": 7.1, "GRÜNE": 8.0}

UNIT_HEADER_RE = re.compile(r"^(\d{1,3})\s+(\S.*)$")
BLOCK_TOKENS = {"08": "L08", "03": "L03", "05": "B05", "04": "E04"}


# =============================================================================
# generic geometry helpers
# =============================================================================
def page_lines(page, ytol=2.5):
    """Words grouped into visual lines, each sorted left to right."""
    ws = page.extract_words()
    ws.sort(key=lambda w: (w["top"], w["x0"]))
    out, cur, cur_top = [], [], None
    for w in ws:
        if cur and w["top"] - cur_top > ytol:
            out.append(sorted(cur, key=lambda t: t["x0"]))
            cur, cur_top = [], None
        if not cur:
            cur_top = w["top"]
        cur.append(w)
    if cur:
        out.append(sorted(cur, key=lambda t: t["x0"]))
    return out


def merge_cells(toks):
    """Merge space-separated German thousands groups into single cells.

    Returns [(text, x1)]. A token is glued onto the running cell when it is
    exactly 3 digits, the cell so far is all-digit, and the gap is small.
    """
    cells = []
    for t in toks:
        txt, x0, x1 = t["text"], t["x0"], t["x1"]
        if cells:
            ptxt, px1 = cells[-1]
            if (re.fullmatch(r"\d{3}", txt) and re.fullmatch(r"\d+", ptxt)
                    and x0 - px1 < MERGE_GAP):
                cells[-1] = (ptxt + txt, x1)
                continue
        cells.append((txt, x1))
    return cells


def to_num(txt):
    if "," in txt:
        return float(txt.replace(",", "."))
    return int(txt)


def classify(x1, anchors, names, where):
    i = min(range(len(anchors)), key=lambda j: abs(anchors[j] - x1))
    if abs(anchors[i] - x1) > EDGE_TOL:
        raise SystemExit(f"[{where}] x1={x1:.1f} does not match any column anchor "
                         f"(closest '{names[i]}' @ {anchors[i]:.1f}, "
                         f"diff {abs(anchors[i] - x1):.2f}pt) - column alignment broke")
    return names[i]


# =============================================================================
# LEFT page: WK/Land headers, block markers (L08/L03/B05/E04), Zahl/% rows
# =============================================================================
def parse_left_page(page, state):
    """Advances `state` (unit/unit_name/block/row_kind) across the whole table;
    returns an ordered list of {'unit','unit_name','block','row_kind','values'}
    for every Zahl/% row on this page (all 4 blocks, not just L08 - needed to
    keep the row SEQUENCE correct for zipping against the unlabelled right
    page)."""
    rows_out = []
    for toks in page_lines(page):
        texts = [t["text"] for t in toks]
        if not texts:
            continue
        x0 = toks[0]["x0"]

        if texts[0] == "Land" and len(texts) > 1 and texts[1] == "Niedersachsen" and x0 < 90:
            state["unit"], state["unit_name"] = "LAND", "Land Niedersachsen"
            state["block"], state["row_kind"] = None, None
            continue

        m = UNIT_HEADER_RE.match(" ".join(texts))
        if m and x0 < 90:
            nr = int(m.group(1))
            if 1 <= nr <= N_WKR:
                state["unit"] = f"{nr:03d}"
                state["unit_name"] = m.group(2).strip()
                state["block"], state["row_kind"] = None, None
                continue

        if state.get("unit") is None:
            continue  # header/title rows before the first WK

        rest = toks
        if len(texts) >= 2 and re.fullmatch(r"[LBE]", texts[0]) and texts[1] in BLOCK_TOKENS:
            state["block"] = BLOCK_TOKENS[texts[1]]
            state["row_kind"] = None
            rest = toks[2:]

        if state.get("block") is None:
            continue

        rtexts = [t["text"] for t in rest]
        if not rtexts:
            continue
        if rtexts[0] == "Zahl":
            if len(rtexts) > 1 and rtexts[1] in ("I", "II"):
                state["row_kind"] = "Zahl" + rtexts[1]
                data = rest[2:]
            else:
                state["row_kind"] = "Zahl"
                data = rest[1:]
        elif rtexts[0] == "%":
            prev = state.get("row_kind")
            nxt = {"ZahlI": "pctI", "ZahlII": "pctII", "Zahl": "pct"}.get(prev)
            if nxt is None:
                continue  # stray '%' with no preceding Zahl row - not a data row
            state["row_kind"] = nxt
            data = rest[1:]
        else:
            continue  # footnote text / page furniture

        where = f"left p{page.page_number} {state['unit']} {state['block']} {state['row_kind']}"
        vals = {}
        for txt, x1 in merge_cells(data):
            if txt in DASH:
                continue
            name = classify(x1, LEFT_ANCHORS, LEFT_COLS, where)
            vals[name] = to_num(txt)
        rows_out.append({"unit": state["unit"], "unit_name": state["unit_name"],
                         "block": state["block"], "row_kind": state["row_kind"],
                         "values": vals})
    return rows_out


# =============================================================================
# RIGHT page: unlabelled continuation - 13 minor-party columns, same row order
# =============================================================================
def parse_right_page(page):
    rows_out = []
    for toks in page_lines(page):
        cells = merge_cells(toks)
        if len(cells) != len(RIGHT_COLS):
            continue
        texts = [c[0] for c in cells]
        if not all((t in DASH) or re.fullmatch(r"\d+(,\d+)?", t) for t in texts):
            continue  # not a pure data row (header/footnote/page number)
        where = f"right p{page.page_number}"
        vals = {}
        for txt, x1 in cells:
            if txt in DASH:
                continue
            name = classify(x1, RIGHT_ANCHORS, RIGHT_COLS, where)
            vals[name] = to_num(txt)
        rows_out.append({"values": vals})
    return rows_out


# =============================================================================
# main
# =============================================================================
def main():
    fails = []

    def req(cond, msg):
        print(("  [ok]   " if cond else "  [FAIL] ") + msg)
        if not cond:
            fails.append(msg)

    print("Reading", os.path.relpath(PDF, ROOT))
    pdf = pdfplumber.open(PDF)

    state = {"unit": None, "unit_name": None, "block": None, "row_kind": None}
    all_rows = []
    pno = PAGE_START
    while pno <= PAGE_END:
        left_page, right_page = pdf.pages[pno], pdf.pages[pno + 1]
        ltxt = left_page.extract_text() or ""
        rtxt = right_page.extract_text() or ""
        if "CDU" not in ltxt:
            sys.exit(f"page {pno}: expected a LEFT page (CDU header) - layout assumption broke")
        if "CDU" in rtxt:
            sys.exit(f"page {pno + 1}: expected a RIGHT page (no CDU header) - layout assumption broke")

        left_rows = parse_left_page(left_page, state)
        right_rows = parse_right_page(right_page)
        if len(left_rows) != len(right_rows):
            sys.exit(f"page pair ({pno},{pno + 1}): row count mismatch "
                     f"left={len(left_rows)} right={len(right_rows)} - "
                     f"left/right halves desynchronised")
        for lr, rr in zip(left_rows, right_rows):
            all_rows.append({**lr, "right_values": rr["values"]})
        pno += 2

    print(f"  parsed {len(all_rows)} tagged rows from {PAGE_END - PAGE_START + 1} pages "
          f"({(PAGE_END - PAGE_START + 1) // 2} page pairs)")

    # ---- structural check: every unit has exactly the expected 14-row pattern
    EXPECTED_SEQ = (["L08:ZahlI", "L08:pctI", "L08:ZahlII", "L08:pctII"]
                    + ["L03:ZahlI", "L03:pctI", "L03:ZahlII", "L03:pctII"]
                    + ["B05:ZahlI", "B05:pctI", "B05:ZahlII", "B05:pctII"]
                    + ["E04:Zahl", "E04:pct"])
    by_unit = {}
    for r in all_rows:
        by_unit.setdefault(r["unit"], []).append(r)

    print("\n=========== VALIDATION (NI 2008, Wahlkreis level, L08 block) ===========")

    want_units = {f"{i:03d}" for i in range(1, N_WKR + 1)} | {"LAND"}
    req(set(by_unit) == want_units,
        f"(1a) exactly {N_WKR} Wahlkreise + Land row present ({len(by_unit)} units)")

    bad_seq = []
    for u, rows in by_unit.items():
        got = [f"{r['block']}:{r['row_kind']}" for r in rows]
        if got != EXPECTED_SEQ:
            bad_seq.append((u, got))
    req(not bad_seq, "(1b) every unit has the expected 14-row L08/L03/B05/E04 sequence")
    for u, got in bad_seq[:5]:
        print("           ", u, got)
    if fails:
        print(f"\n{len(fails)} VALIDATION FAILURE(S) - nothing written.")
        sys.exit(1)

    # ---- build per-(unit,stimme) records from the L08 block only -----------
    STIMME_OF = {"ZahlI": "erststimme", "ZahlII": "zweitstimme"}
    rec = {}  # (unit, stimme) -> dict
    unit_name = {u: rows[0]["unit_name"] for u, rows in by_unit.items()}
    unit_wb_wa = {}  # unit -> (eligible, number), from L08 ZahlI
    for u, rows in by_unit.items():
        l08 = {r["row_kind"]: r for r in rows if r["block"] == "L08"}
        unit_wb_wa[u] = (l08["ZahlI"]["values"].get("eligible_voters"),
                        l08["ZahlI"]["values"].get("number_voters"))

    l08_rows = [r for r in all_rows if r["block"] == "L08"]
    for r in l08_rows:
        u, rk = r["unit"], r["row_kind"]
        if rk not in STIMME_OF:
            continue
        stimme = STIMME_OF[rk]
        wb, wa = unit_wb_wa[u]
        invalid = r["values"].get("invalid_votes")
        valid = r["values"].get("valid_votes")
        parties = {}
        for p in LEFT_COLS[4:]:
            parties[p] = r["values"].get(p)
        for p in RIGHT_COLS:
            parties[p] = r["right_values"].get(p)
        rec[(u, stimme)] = {"eligible_voters": wb, "number_voters": wa,
                            "invalid_votes": invalid, "valid_votes": valid,
                            "parties": parties}

    req(len(rec) == len(want_units) * 2,
        f"(1c) {len(rec)} (unit,stimme) L08 records built (expected {len(want_units) * 2})")

    # (2) per (WK, stimme): sum of all 18 party columns == gueltige Stimmen ---
    bad = []
    for (u, stimme), r in rec.items():
        if u == "LAND":
            continue
        s = sum(v for v in r["parties"].values() if v is not None)
        if s != r["valid_votes"]:
            bad.append((u, stimme, s, r["valid_votes"]))
    req(not bad, f"(2) every (WK,stimme): sum of all 18 party columns == gültige Stimmen "
                 f"exactly ({N_WKR * 2} groups)")
    for b in bad[:8]:
        print("           ", b)

    # (3) per party, both stimme: sum over 87 WK == printed Land row ---------
    bad = []
    for stimme in ("erststimme", "zweitstimme"):
        land = rec[("LAND", stimme)]["parties"]
        for p in ALL_PARTY_COLS:
            mine = sum((rec[(f"{i:03d}", stimme)]["parties"].get(p) or 0)
                      for i in range(1, N_WKR + 1))
            ref = land.get(p) or 0
            if mine != ref:
                bad.append((stimme, p, mine, ref))
    req(not bad, "(3) every party's 87-Wahlkreis sum == the report's own Land Niedersachsen "
                 f"row, exactly, both stimme ({2 * len(ALL_PARTY_COLS)} checks)")
    for b in bad[:12]:
        print("           ", b)

    # (4) Wähler == gültige + ungültige, per (unit, stimme) -------------------
    bad = []
    for (u, stimme), r in rec.items():
        wa = r["number_voters"]
        if wa is None or r["invalid_votes"] is None or r["valid_votes"] is None:
            bad.append((u, stimme, "missing turnout field"))
            continue
        if wa != r["invalid_votes"] + r["valid_votes"]:
            bad.append((u, stimme, wa, r["invalid_votes"], r["valid_votes"]))
    req(not bad, f"(4) every (unit,stimme): Wähler == gültige + ungültige "
                 f"({len(rec)} groups)")
    for b in bad[:8]:
        print("           ", b)

    # (5) pinned official Zweitstimme statewide shares, +-0.1pp --------------
    land_z = rec[("LAND", "zweitstimme")]
    gz = land_z["valid_votes"]
    bad = []
    for p, official in OFFICIAL_ZWEITSTIMME.items():
        v = land_z["parties"].get(p) or 0
        got = 100.0 * v / gz
        if abs(got - official) > 0.1:
            bad.append((p, round(got, 2), official))
    req(not bad, "(5) pinned official Zweitstimme shares match the Land Niedersachsen row "
                 "(+-0.1pp): CDU 42.5, SPD 30.3, FDP 8.2, DIE LINKE 7.1, GRÜNE 8.0")
    for b in bad:
        print("           ", b)
    print(f"    Land Zweitstimme shares: " +
          ", ".join(f"{p}={100.0 * (land_z['parties'].get(p) or 0) / gz:.2f}%"
                    for p in OFFICIAL_ZWEITSTIMME))

    if fails:
        print(f"\n{len(fails)} VALIDATION FAILURE(S) - nothing written.")
        sys.exit(1)

    # ---- which of the 18 party columns actually appear on the 2008 ballot --
    on_ballot = [p for p in ALL_PARTY_COLS
                if any((rec[(f"{i:03d}", s)]["parties"].get(p) is not None)
                       for i in range(1, N_WKR + 1) for s in ("erststimme", "zweitstimme"))]
    off_ballot = [p for p in ALL_PARTY_COLS if p not in on_ballot]
    print(f"\nParties with >=1 vote somewhere (87 WK, both stimme): {len(on_ballot)} / {len(ALL_PARTY_COLS)}")
    print("  on ballot :", on_ballot)
    if off_ballot:
        print("  NEVER voted for (dropped from output):", off_ballot)

    # --- emit ------------------------------------------------------------------
    rows = []
    for i in range(1, N_WKR + 1):
        u = f"{i:03d}"
        for stimme in ("erststimme", "zweitstimme"):
            r = rec[(u, stimme)]
            for p in on_ballot:
                v = r["parties"].get(p)
                rows.append({
                    "state_abbr": STATE_ABBR, "state": STATE_NAME,
                    "election_year": ELECTION_YEAR, "election_date": ELECTION_DATE,
                    "wkr_nr": u, "wkr_name": unit_name[u], "stimme": stimme,
                    "eligible_voters": r["eligible_voters"],
                    "number_voters": r["number_voters"],
                    "valid_votes": r["valid_votes"],
                    "invalid_votes": r["invalid_votes"],
                    "party_raw": p, "votes": "" if v is None else v,
                })
    os.makedirs(OUT_DIR, exist_ok=True)
    with open(OUT, "w", newline="", encoding="utf-8") as fh:
        wr = csv.DictWriter(fh, fieldnames=list(rows[0].keys()))
        wr.writeheader()
        wr.writerows(rows)
    print(f"\nWrote {len(rows)} rows -> {os.path.relpath(OUT, ROOT)}")
    for stimme in ("erststimme", "zweitstimme"):
        n = sum(1 for r in rows if r["stimme"] == stimme)
        print(f"  {stimme:12s}: {n} rows ({n // N_WKR} parties x {N_WKR} Wahlkreise)")


if __name__ == "__main__":
    main()
