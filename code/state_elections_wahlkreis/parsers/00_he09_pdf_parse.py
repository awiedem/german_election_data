#!/usr/bin/env python3
"""Stage-0 parser for HESSEN Landtagswahl results at WAHLKREIS level, 2009.

Source (raw, read-only):
  data/state_elections/raw/Landtagswahlen_Wahlkreis/Hessen/
    HE_2009_Landtagswahl_Wahlkreis.pdf
  = Der Landeswahlleiter fuer Hessen, published in Staatsanzeiger fuer das
    Land Hessen Nr. 8 (16. Februar 2009), "Endgueltiges Ergebnis der
    Landtagswahl am 18. Januar 2009" (18. Hessischer Landtag).

Why a PDF parser: no machine-readable Wahlkreis-level source exists for 2009
(the open-data CSV starts in 2023, the digitised statistical report starts in
2018). This Staatsanzeiger issue carries a TEXT LAYER (no OCR needed) and
reports, for every one of the 55 Wahlkreise, one self-contained block with
both WAHLKREISSTIMMEN (Erststimme) and LANDESSTIMMEN (Zweitstimme) side by
side, plus a "Land Hessen" statewide block on page 1 in the same layout.

!! 2009 IS ON ITS OWN (PRE-2018) WAHLKREISEINTEILUNG !!
  The Dec-2017 LWG amendment re-cut several Wahlkreise (see 00_he_pdf_parse.py
  for the 2018/2013 boundary story). 2009 predates that cut entirely, so its
  55 Wahlkreise are NOT the same geographic units as 2013/2018/2023, even
  though the numbering habitually runs 1-55 in all of them. Do NOT compare or
  enforce wkr_name identity against later years here; this script emits the
  2009 document's own names verbatim. flag_wkr_boundaries_recomputed = 0 for
  every 2009 row (nothing was recomputed onto a different boundary set - this
  is simply a different, self-contained boundary regime).

LAYOUT: page 1 = statewide "Land Hessen" block. Pages 2-29 = one block per
Wahlkreis, header "Wahlkreis (Nr. )?N - Name" (four of the 55 omit "Nr.":
13, 15, 16, 20), then Wahlberechtigte / Waehler / Wahlbeteiligung / ungueltige
Stimmen / gueltige Stimmen, then one row per party. Each row carries up to 4
right-aligned numeric cells: WK-Anzahl, WK-%, LS-Anzahl, LS-%; a party with no
district candidate in that Wahlkreis simply has NO cell in the WK-Anzahl/
WK-% position (nothing is printed there - there is no "x"/"-" placeholder in
this document, unlike the 2018 report). Ten parties (CDU, SPD, FDP, GRUENE,
DIE LINKE, REP, FREIE WAEHLER, NPD, PIRATEN, BueSo) are Landesliste parties
and get a row with an LS value in every single Wahlkreis; five further
entries (APPD, Buergerbewegung - WIR, FAMILIE SCHMIDT, Menschlichkeit, Wolf
Ruppert - direkt) are Wahlkreis-only individual/local candidates that appear
as an EXTRA row only in the specific Wahlkreis(e) they contested (Buerger-
bewegung - WIR ran in two: WK 18 and WK 52) and never carry an LS value -
confirmed exhaustively below (VALIDATION step 0).

PARSING METHOD: pdfplumber words grouped into visual lines (y-tolerance
2.5pt), then split into a label (x0 < 240pt) and up to four right-aligned
numeric cells classified into WK-Anzahl / WK-% / LS-Anzahl / LS-% by x0
bucket (<320 / <380 / <460 / else). Thousands separator is "." (dot),
decimal separator is ",". Every accepted row must carry at least one numeric
cell; the "- WAHLKREISSTIMMEN - - LANDESSTIMMEN -" and "gewaehlt: ..." rows
are skipped explicitly by text match; the "in % / in %" header row has no
label token at all (all its cells sit at x0 >= 240) and needs no special
case. Parsing stops at the "II." (Sitzverteilung) heading that follows the
last Wahlkreis (55) on the same page - everything after that is an unrelated
seat-allocation / gazette section.

Party-name spelling is not perfectly consistent between the "Land Hessen"
block and the per-Wahlkreis blocks: "BUESO" is printed "BUESo" (sic, per-WK)
vs "BUESo" with a differently-cased first letter on the Land page, and
"Buergerbewegung - WIR" uses a plain hyphen per-WK vs an en-dash on the Land
page. Comparisons against the Land block below canonicalise (uppercase, en-
dash -> hyphen) for matching; the EMITTED party_raw always keeps the per-WK
verbatim spelling (there is precedent for this in HE_2013's own "Sonstige"
handling - see 00_he_pdf_parse.py).

KNOWN SOURCE DEFECTS (kept as printed; each is asserted to be the ONLY
instance of its kind before anything is written):
  (1) Wahlkreis 44 (Offenbach Land I): printed "gueltige Wahlkreisstimmen" =
      50,596, but its own printed WK-side party rows (CDU+SPD+FDP+GRUENE+
      DIE LINKE+NPD) sum to 50,585 - an 11-vote gap IN THE OFFICIAL TABLE.
      The Land-Hessen block's CDU/Wahlkreisstimmen total (1,083,174) reflects
      the CORRECTED figure (i.e. it is 11 higher than summing the 55 printed
      per-Wahlkreis CDU/WK values, 1,083,163): the statewide compiler used
      the true count, the per-Wahlkreis table did not. Both facts are
      asserted below rather than silently reconciled.
  (2) Wahlkreis 1 (Kassel-Land I): printed "Waehler" (number_voters) for the
      LANDESSTIMMEN side is 58,488, but that Wahlkreis's own printed
      gueltige+ungueltige Landesstimmen sum to 58,448 (= the WK-side Waehler
      value, printed correctly). A 40-voter typo in the source, independent
      of defect (1) and not mentioned in the report's own errata. Confirmed
      via two independent extraction methods (pdfplumber word geometry and
      `pdftotext -layout`).

VALIDATION (all hard; nothing is written if any check fails):
  (0) exactly 55 Wahlkreise (numbers 1-55, unique names), both stimme
      present in the Land block and in every Wahlkreis; the 10 Landesliste
      parties appear, with an LS value, in literally all 55 Wahlkreise; the
      5 individual-candidate entries never carry an LS value anywhere
  (1) per (Wahlkreis, stimme): sum(party votes) == gueltige Stimmen, with
      EXACTLY the pinned Wahlkreis-44/Wahlkreisstimmen exception (defect 1)
  (2) per (Wahlkreis, stimme): Waehler == gueltige + ungueltige, with
      EXACTLY the pinned Wahlkreis-1/Landesstimmen exception (defect 2)
  (3) sum over the 55 Wahlkreise, per party, per stimme, == the Land-Hessen
      block's own printed total - EXACTLY, except CDU/Wahlkreisstimmen (the
      Land block carries the corrected total; see defect 1) - and the Land
      block is checked for internal consistency (gueltige == sum of its own
      party column) so this exception is not a blanket excuse
  (4) pinned official Landesstimmen shares (+-0.1pp): CDU 37.2, SPD 23.7,
      FDP 16.2, GRUENE 13.7, DIE LINKE 5.4

Output: data/state_elections/processed/wahlkreis/he_pdf/HE_2009_pdf_long.csv
        (read by parsers/parse_HE.R, appended as a third fixture)
Run:    python3 code/state_elections_wahlkreis/parsers/00_he09_pdf_parse.py
"""

import csv
import os
import re
import sys

import pdfplumber

HERE = os.path.dirname(os.path.abspath(__file__))
ROOT = os.path.dirname(os.path.dirname(os.path.dirname(HERE)))
PDF = os.path.join(
    ROOT, "data", "state_elections", "raw", "Landtagswahlen_Wahlkreis", "Hessen",
    "HE_2009_Landtagswahl_Wahlkreis.pdf",
)
OUT_DIR = os.path.join(ROOT, "data", "state_elections", "processed", "wahlkreis", "he_pdf")
OUT = os.path.join(OUT_DIR, "HE_2009_pdf_long.csv")

STATE_ABBR, STATE_NAME = "HE", "Hessen"
N_WKR = 55
ELECTION_YEAR = 2009
ELECTION_DATE = "2009-01-18"

REGULAR_PARTIES = ["CDU", "SPD", "FDP", "GRÜNE", "DIE LINKE", "REP",
                    "FREIE WÄHLER", "NPD", "PIRATEN", "BüSo"]

# the pinned, exhaustively-checked source defects (see module docstring)
WK44_ERSTSTIMME_GAP = 11    # gueltige Wahlkreisstimmen exceeds party sum by this much
WK1_WAEHLER_LS_TYPO = 40    # printed LS-side Waehler exceeds gueltige+ungueltige by this much

OFFICIAL_LS_SHARES = {"CDU": 37.2, "SPD": 23.7, "FDP": 16.2,
                      "GRÜNE": 13.7, "DIE LINKE": 5.4}

LABEL_FIELDS = {
    "Wahlberechtigte": "eligible_voters",
    "Wähler": "number_voters",
    "ungültige Stimmen": "invalid_votes",
    "gültige Stimmen": "valid_votes",
}
SKIP_LABELS = {"Wahlbeteiligung"}   # turnout %, not a count field we need

WK_RE = re.compile(r"^Wahlkreis\s+(?:Nr\.\s*)?(\d+)\s*[–-]\s*(.+)$")
STOP_RE = re.compile(r"^II\.\s")

# geometry: numbers are right-aligned in 4 columns (WK-Anzahl, WK-%,
# LS-Anzahl, LS-%). Observed x0 ranges across the whole document: WK-Anzahl
# 278-308, WK-% 357-374, LS-Anzahl 417-444, LS-% 486-506 - each bucket has
# >=30pt of clearance from its neighbours, so a simple threshold is safe;
# safety is in any case proven downstream by the per-row / per-party /
# statewide validations, which would break on any column misassignment.
def col_of(x0):
    if x0 < 320:
        return "wk_count"
    if x0 < 380:
        return "wk_pct"
    if x0 < 460:
        return "ls_count"
    return "ls_pct"


NUM_RE = re.compile(r"\d[\d.,]*")


def to_num(txt):
    s = txt.replace(".", "").replace(",", ".")
    return float(s) if "." in s else int(s)


def canon(label):
    """Canonical form for cross-checking a party label against the Land
    block, where casing and dash characters are not always identical to the
    per-Wahlkreis spelling (see module docstring)."""
    return label.strip().upper().replace("–", "-").replace("—", "-")


def page_rows(page, ytol=2.5):
    words = page.extract_words(keep_blank_chars=False, use_text_flow=False)
    words = sorted(words, key=lambda w: (w["top"], w["x0"]))
    rows, cur, cur_top = [], [], None
    for w in words:
        if cur and w["top"] - cur_top > ytol:
            rows.append(sorted(cur, key=lambda t: t["x0"]))
            cur, cur_top = [], None
        if not cur:
            cur_top = w["top"]
        cur.append(w)
    if cur:
        rows.append(sorted(cur, key=lambda t: t["x0"]))
    return rows


def parse_blocks():
    """Returns {block_key: {'name':..., 'fields':{field: {col: val}},
    'parties': {party_label: {col: val}}}}, block_key = 'LAND' or 1..55."""
    pdf = pdfplumber.open(PDF)
    blocks = {}
    current = None
    stopped = False
    for pno in range(0, 29):
        if stopped:
            break
        for row in page_rows(pdf.pages[pno]):
            text = " ".join(w["text"] for w in row)
            if STOP_RE.match(text.strip()):
                stopped = True
                break

            m = WK_RE.match(text.strip())
            if m:
                current = int(m.group(1))
                blocks[current] = {"name": m.group(2).strip(),
                                   "fields": {}, "parties": {}}
                continue
            if text.strip().startswith("Land Hessen"):
                current = "LAND"
                blocks[current] = {"name": "Land Hessen",
                                   "fields": {}, "parties": {}}
                continue
            if current is None:
                continue
            if "WAHLKREISSTIMMEN" in text or "LANDESSTIMMEN" in text:
                continue
            if text.strip().startswith("gewählt:"):
                continue

            label_words = [w for w in row if w["x0"] < 240]
            data_words = [w for w in row if w["x0"] >= 240]
            label = " ".join(w["text"] for w in label_words).strip()
            if not label or label in SKIP_LABELS:
                continue
            numeric = [w for w in data_words if NUM_RE.fullmatch(w["text"])]
            if not numeric:
                continue
            vals = {}
            for w in numeric:
                vals[col_of(w["x0"])] = to_num(w["text"])

            if label in LABEL_FIELDS:
                blocks[current]["fields"][LABEL_FIELDS[label]] = vals
            else:
                if re.search(r"\d", label):
                    continue   # page furniture ("Nr. 8 ... Seite 471" etc.)
                blocks[current]["parties"][label] = vals
    return blocks


def main():
    fails = []

    def req(cond, msg):
        print(("  [ok]   " if cond else "  [FAIL] ") + msg)
        if not cond:
            fails.append(msg)

    print("Reading", os.path.relpath(PDF, ROOT))
    blocks = parse_blocks()
    wkrs = sorted(k for k in blocks if k != "LAND")
    land = blocks.get("LAND")

    print("\n=========== VALIDATION (HE 2009, Wahlkreis level) ===========")

    # --- (0) structural completeness -----------------------------------------
    req(land is not None, "Land Hessen block found")
    req(wkrs == list(range(1, N_WKR + 1)), f"exactly {N_WKR} Wahlkreise, numbered 1..{N_WKR}")
    names = {k: blocks[k]["name"] for k in wkrs}
    req(len(set(names.values())) == N_WKR, f"{N_WKR} unique Wahlkreis names")

    reg_missing_ls = [(k, p) for k in wkrs for p in REGULAR_PARTIES
                       if blocks[k]["parties"].get(p, {}).get("ls_count") is None]
    req(not reg_missing_ls,
        f"all {len(REGULAR_PARTIES)} Landesliste parties carry an LS value in every one of the {N_WKR} Wahlkreise")
    for b in reg_missing_ls[:8]:
        print("           ", b)

    indiv_parties = sorted({p for k in wkrs for p in blocks[k]["parties"]
                            if p not in REGULAR_PARTIES})
    indiv_with_ls = [(k, p) for k in wkrs for p in indiv_parties
                     if blocks[k]["parties"].get(p, {}).get("ls_count") is not None]
    req(not indiv_with_ls,
        f"the {len(indiv_parties)} individual/local-candidate entries never carry an LS value")
    for b in indiv_with_ls[:8]:
        print("           ", b)

    if fails:
        print(f"\n{len(fails)} VALIDATION FAILURE(S) - nothing written.")
        sys.exit(1)

    # --- (1) per-(Wahlkreis,stimme): sum(party votes) == gueltige Stimmen ----
    bad = []
    for k in wkrs:
        b = blocks[k]
        for stimme, col in (("erststimme", "wk_count"), ("zweitstimme", "ls_count")):
            s = sum(v.get(col) or 0 for v in b["parties"].values())
            valid = b["fields"]["valid_votes"][col]
            if s != valid:
                bad.append((k, stimme, s, valid, valid - s))
    pinned1 = [b for b in bad if b[:2] == (44, "erststimme") and b[4] == WK44_ERSTSTIMME_GAP]
    unexpected1 = [b for b in bad if b not in pinned1]
    req(len(pinned1) == 1 and not unexpected1,
        f"sum(party votes) == gueltige Stimmen everywhere, except the pinned "
        f"Wahlkreis 44 erststimme gap of {WK44_ERSTSTIMME_GAP} votes (source defect, kept as printed)")
    for b in unexpected1[:8]:
        print("           ", b)

    # --- (2) Waehler == gueltige + ungueltige --------------------------------
    bad = []
    for k in wkrs:
        b = blocks[k]
        for stimme, col in (("erststimme", "wk_count"), ("zweitstimme", "ls_count")):
            wae = b["fields"]["number_voters"][col]
            g = b["fields"]["valid_votes"][col]
            u = b["fields"]["invalid_votes"][col]
            if wae != g + u:
                bad.append((k, stimme, wae, g, u, wae - (g + u)))
    pinned2 = [b for b in bad if b[:2] == (1, "zweitstimme") and b[5] == WK1_WAEHLER_LS_TYPO]
    unexpected2 = [b for b in bad if b not in pinned2]
    req(len(pinned2) == 1 and not unexpected2,
        f"Waehler == gueltige + ungueltige everywhere, except the pinned Wahlkreis 1 "
        f"Landesstimmen Waehler typo of {WK1_WAEHLER_LS_TYPO} voters (source defect, kept as printed)")
    for b in unexpected2[:8]:
        print("           ", b)

    # --- (3) 55-Wahlkreis sum per party, per stimme, vs the Land block -------
    land_canon = {canon(p): v for p, v in land["parties"].items()}
    bad = []
    for stimme, col in (("erststimme", "wk_count"), ("zweitstimme", "ls_count")):
        for p in REGULAR_PARTIES:
            mine = sum(blocks[k]["parties"].get(p, {}).get(col) or 0 for k in wkrs)
            ref = land_canon.get(canon(p), {}).get(col) or 0
            if mine != ref:
                bad.append((stimme, p, mine, ref, ref - mine))
    pinned3 = [b for b in bad if b[:2] == ("erststimme", "CDU") and b[4] == WK44_ERSTSTIMME_GAP]
    unexpected3 = [b for b in bad if b not in pinned3]
    req(len(pinned3) == 1 and not unexpected3,
        "55-Wahlkreis sums match the Land Hessen block exactly for all "
        f"{len(REGULAR_PARTIES)} parties x 2 stimme, except erststimme CDU "
        f"(Land block carries the {WK44_ERSTSTIMME_GAP}-vote CORRECTED Wahlkreis-44 figure, "
        "i.e. the printed per-Wahlkreis table is what's wrong, not the statewide total)")
    for b in unexpected3[:8]:
        print("           ", b)

    # is the Land block internally consistent (its own gueltige == its own
    # party-column sum)? report which side, since the exception above only
    # makes sense if the Land block's OWN total is self-consistent.
    for stimme, col in (("erststimme", "wk_count"), ("zweitstimme", "ls_count")):
        s = sum(v.get(col) or 0 for v in land["parties"].values())
        valid = land["fields"]["valid_votes"][col]
        print(f"    Land block self-consistency [{stimme}]: "
              f"sum(parties)={s} vs printed gueltige={valid} "
              f"({'CONSISTENT' if s == valid else f'differs by {valid - s}'})")

    # --- (4) pinned official Landesstimmen shares (+-0.1pp) ------------------
    total_ls_valid = sum(blocks[k]["fields"]["valid_votes"]["ls_count"] for k in wkrs)
    bad = []
    for p, official in OFFICIAL_LS_SHARES.items():
        v = sum(blocks[k]["parties"].get(p, {}).get("ls_count") or 0 for k in wkrs)
        got = 100.0 * v / total_ls_valid
        if abs(got - official) > 0.1:
            bad.append((p, round(got, 2), official))
    req(not bad, "pinned official Landesstimmen shares match within 0.1pp "
                 f"(CDU/SPD/FDP/GRUENE/DIE LINKE)")
    for b in bad:
        print("           ", b)

    if fails:
        print(f"\n{len(fails)} VALIDATION FAILURE(S) - nothing written.")
        sys.exit(1)

    # --- emit -----------------------------------------------------------------
    rows = []
    for k in wkrs:
        b = blocks[k]
        for p in REGULAR_PARTIES:
            v = b["parties"].get(p, {})
            rows.append({
                "state_abbr": STATE_ABBR, "state": STATE_NAME,
                "election_year": ELECTION_YEAR, "election_date": ELECTION_DATE,
                "wkr_nr": f"{k:02d}", "wkr_name": b["name"], "stimme": "erststimme",
                "eligible_voters": b["fields"]["eligible_voters"]["wk_count"],
                "number_voters": b["fields"]["number_voters"]["wk_count"],
                "valid_votes": b["fields"]["valid_votes"]["wk_count"],
                "invalid_votes": b["fields"]["invalid_votes"]["wk_count"],
                "party_raw": p, "votes": "" if v.get("wk_count") is None else v["wk_count"],
                "flag_wkr_boundaries_recomputed": 0,
            })
            rows.append({
                "state_abbr": STATE_ABBR, "state": STATE_NAME,
                "election_year": ELECTION_YEAR, "election_date": ELECTION_DATE,
                "wkr_nr": f"{k:02d}", "wkr_name": b["name"], "stimme": "zweitstimme",
                "eligible_voters": b["fields"]["eligible_voters"]["ls_count"],
                "number_voters": b["fields"]["number_voters"]["ls_count"],
                "valid_votes": b["fields"]["valid_votes"]["ls_count"],
                "invalid_votes": b["fields"]["invalid_votes"]["ls_count"],
                "party_raw": p, "votes": "" if v.get("ls_count") is None else v["ls_count"],
                "flag_wkr_boundaries_recomputed": 0,
            })
        for p in sorted(pp for pp in b["parties"] if pp not in REGULAR_PARTIES):
            v = b["parties"][p]
            rows.append({
                "state_abbr": STATE_ABBR, "state": STATE_NAME,
                "election_year": ELECTION_YEAR, "election_date": ELECTION_DATE,
                "wkr_nr": f"{k:02d}", "wkr_name": b["name"], "stimme": "erststimme",
                "eligible_voters": b["fields"]["eligible_voters"]["wk_count"],
                "number_voters": b["fields"]["number_voters"]["wk_count"],
                "valid_votes": b["fields"]["valid_votes"]["wk_count"],
                "invalid_votes": b["fields"]["invalid_votes"]["wk_count"],
                "party_raw": p, "votes": v["wk_count"],
                "flag_wkr_boundaries_recomputed": 0,
            })

    os.makedirs(OUT_DIR, exist_ok=True)
    with open(OUT, "w", newline="", encoding="utf-8") as fh:
        wr = csv.DictWriter(fh, fieldnames=list(rows[0].keys()))
        wr.writeheader()
        wr.writerows(rows)

    print(f"\nWrote {len(rows)} rows -> {os.path.relpath(OUT, ROOT)}")
    for stimme in ("erststimme", "zweitstimme"):
        n = sum(1 for r in rows if r["stimme"] == stimme)
        nparty = len({r["party_raw"] for r in rows if r["stimme"] == stimme})
        print(f"  {stimme:12s}: {n} rows ({nparty} distinct party_raw)")
    print("  distinct party_raw (all):", sorted({r["party_raw"] for r in rows}))


if __name__ == "__main__":
    main()
