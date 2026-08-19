#!/usr/bin/env python3
"""Stage-0 parser for BREMEN (HB) Buergerschaftswahl results at WAHLBEREICH
level, 2003 + 2007 + 2011 + 2023 -- the four "Statistische Mitteilungen" Hefte
that are NOT already covered by parse_HB.R's votemanager OpenData CSVs
(2015, 2019).

Source (raw, read-only), all under
  data/state_elections/raw/Landtagswahlen_Wahlkreis/Bremen/:
    HB_2003_Buergerschaftswahl_Heft106_Bericht.pdf
    HB_2007_Buergerschaftswahl_Heft110_Bericht.pdf
    HB_2011_Buergerschaftswahl_Heft113_Teil1_Analysen_Tabellen.pdf
    HB_2023_Buergerschaftswahl_Heft126_Bericht_Ortsteile.pdf
  = Statistisches Landesamt Bremen, "Statistische Mitteilungen" Hefte 106 /
    110 / 113 (Teil 1) / 126. All four carry a digital text layer (no OCR).
  Also (2023 only, cross-check source, NOT a counts source):
    HB_2023_Buergerschaftswahl_Wahlkreis_Ortsteile_data.js
  = InstantAtlas "Bremer Wahlatlas" underlying JSON (shares only).

WHERE THE TABLE LIVES (front-matter Inhaltsverzeichnis page numbers do NOT
match pdftotext pagination -- the PDF page must be located by grepping the
body text, not by trusting the printed table of contents):
  2003: pdftotext page 34, "Tab. 1 / Wahl zur Bremischen Buergerschaft
        (Landtag) im Lande Bremen am 25. Mai 2003 / Vorlaeufige Ergebnisse"
        (the "Gesamtuebersichten" section's single-year table; a second,
        near-identical "Tab. 1" earlier in the front matter is a 2003-vs-1999
        COMPARISON table and is intentionally NOT used).
  2007: pdftotext page 39, "Tab. 1 / ... am 13. Mai 2007 / Vorlaeufige
        Ergebnisse" (same "Gesamtuebersichten" pattern as 2003).
  2011: pdftotext page 53, "Tab. 1 / ... im Land Bremen am 22. Mai 2011 /
        Endgueltige Ergebnisse" (there is also an earlier, row-equivalent
        "Amtliches Endergebnis" page (PDF p. 50) using full official party
        names + "Liste N:" labels instead of short codes; both encode the
        identical numbers, Tab. 1's short party codes were used because they
        parse unambiguously).
  2023: pdftotext page 29 (0-idx 28), "Tabelle 1 / ... im Land Bremen am 14.
        Mai 2023 / Endgueltiges Ergebnis" (an identical-titled TOC entry on
        an earlier page is skipped by requiring "Wahlberechtigte insgesamt"
        as an additional anchor string).

NEITHER 2003 NOR 2007's Heft ever prints the word "endgueltig" -- both call
their Tab. 1 "Vorlaeufige Ergebnisse" (preliminary). No later "endgueltig"
table exists in either Heft. These preliminary tables are nevertheless the
official record this source publishes, and they are internally exact (party
counts sum exactly to Gueltige Stimmen, see VALIDATION) and match the pinned
official statewide shares to within 0.13pp -- so they are used as-is.

VOTE SYSTEM, per year:
  2003, 2007: ONE list vote (Listenstimme). "Von den gueltigen Stimmen
    entfielen auf <Partei>: Anzahl / %" -- one row per party, one Anzahl
    per Wahlbereich. valid_votes = "Gueltige Stimmen" directly (Stimmen and
    Stimmzettel coincide under a 1-vote system).
  2011, 2023: FIVE votes per voter (Bremen's post-2011 personalisiertes
    Verhaeltniswahlrecht). Each party gets three sub-rows per Wahlbereich:
    Listenstimmen (L) + Personenstimmen (P) = Zusammen (Z). Z is the party's
    total and is what parse_HB.R's 2015/2019 legend also uses (D-code
    "_SUMME_LISTE_KANDIDATEN" = Listenstimmen + Personenstimmen). Only Z is
    emitted. valid_votes = gueltige STIMMEN ("Insgesamt"/Z row under
    "Gueltige Stimmen / Sitze"), NOT "Gueltige Stimmzettel" (one Stimmzettel
    carries 5 Stimmen) -- mirrors the D2 semantics documented in parse_HB.R.
  ALL FOUR YEARS map to stimme = "zweitstimme" per the single-ballot
  convention documented in parse_HB.R's header (Bremen has only one ballot
  type; there is no separate Erst-/Zweitstimme distinction to preserve).

PARSING METHOD: `pdftotext -layout`, then split each line on runs of 2+
whitespace characters (`\\s{2,}`). This is safe for column separation *and*
for German thousands-grouped numbers (e.g. "1 115 686") because -layout pads
inter-column gaps with several spaces while a genuine thousands separator
within one printed number is exactly one space -- verified empirically (no
false merges/splits across ~30 rows x 4 years) and proven correct downstream
by the sum-of-parties == Gueltige-Stimmen identity (VALIDATION 2), which
would break immediately on any digit-grouping error.

For the 2011/2023 nine-column rows (Anzahl/%/Sitze x 3 Wahlbereiche), only
each triplet's Anzahl (index 0, 3, 6) is parsed to int; %/Sitze tokens are
never numerically parsed (avoids the comma-decimal vs plain-"100" vs
Sitze-adjacent-to-next-Anzahl ambiguities that a blanket parse would hit).
Party name labels that wrap across the L/P/Z sub-rows (e.g. "Dialog" /
"Grundeinkommen", "FREIE" / "WAEHLER" / "BREMEN", "Fuer" / "Bremerhaven",
"Partei fuer schulmedizinische" / "Verjuengungsforschung") are reassembled by
collecting every pre-marker token across the group's rows in order.

VALIDATION (all hard; nothing is written if any check fails):
  (1) both Wahlbereiche (Bremen, Bremerhaven) + the Land Bremen block are
      present for every year (structural: all three are parsed from the same
      row, so this holds whenever the row itself is found).
  (2) per Wahlbereich (Bremen, Bremerhaven, Land Bremen): sum of party votes
      == valid_votes EXACTLY, for every year.
  (3) Wahlbereich Bremen + Wahlbereich Bremerhaven == Land Bremen EXACTLY,
      per party and per turnout field (eligible/number_voters/invalid/valid).
  (4) pinned official statewide (Land Bremen) shares, +-0.15pp:
        2003 SPD 42.3, CDU 29.8, GRUENE 12.8, FDP 4.2
        2007 SPD 36.7, CDU 25.6, GRUENE 16.5, LINKE 8.4, FDP 6.0
        2011 SPD 38.6, GRUENE 22.5, CDU 20.4, LINKE 5.6
        2023 SPD 29.8, CDU 26.2, GRUENE 11.9, LINKE 10.9, BIW 9.4, FDP 5.1
  (5) 2023 only: recomputed per-Wahlbereich party shares reproduce the
      InstantAtlas .js "Parteien: Stimmenverteilung" comparisonValues
      (Bremen/Bremerhaven/Land) within 0.1pp, for every one of the 16
      parties in Tabelle 1 (full coverage, checked and reported).

Output: data/state_elections/processed/wahlkreis/hb_pdf/HB_2003_2023_pdf_long.csv
        (read by parsers/parse_HB.R, which appends its own 2015/2019 rows)
Run:    python3 code/state_elections_wahlkreis/parsers/00_hb_pdf_parse.py
Requires: poppler (pdftotext) on PATH.
"""

import csv
import json
import os
import re
import subprocess
import sys

HERE = os.path.abspath(os.path.dirname(__file__))
ROOT = os.path.dirname(os.path.dirname(os.path.dirname(HERE)))
RAW_DIR = os.path.join(ROOT, "data", "state_elections", "raw",
                        "Landtagswahlen_Wahlkreis", "Bremen")
OUT_DIR = os.path.join(ROOT, "data", "state_elections", "processed",
                        "wahlkreis", "hb_pdf")
OUT = os.path.join(OUT_DIR, "HB_2003_2023_pdf_long.csv")

STATE_ABBR = "HB"
STATE_NAME = "Bremen"
STIMME = "zweitstimme"

# wkr_nr / wkr_name EXACTLY as parse_HB.R's 2015/2019 rows use them.
WKR_NR = {"Bremen": "01", "BHV": "02"}
WKR_NAME = {"Bremen": "Stadt Bremen", "BHV": "Stadt Bremerhaven"}

ELECTION_DATES = {2003: "2003-05-25", 2007: "2007-05-13",
                   2011: "2011-05-22", 2023: "2023-05-14"}

PINNED = {
    2003: {"SPD": 42.3, "CDU": 29.8, "GRÜNE": 12.8, "FDP": 4.2},
    2007: {"SPD": 36.7, "CDU": 25.6, "GRÜNE": 16.5, "Die Linke.": 8.4, "FDP": 6.0},
    2011: {"SPD": 38.6, "GRÜNE": 22.5, "CDU": 20.4, "DIE LINKE": 5.6},
    2023: {"SPD": 29.8, "CDU": 26.2, "GRÜNE": 11.9, "DIE LINKE": 10.9,
           "BIW": 9.4, "FDP": 5.1},
}
PIN_TOL = 0.15

# ---------------------------------------------------------------------------
# low-level helpers
# ---------------------------------------------------------------------------
SPLIT_RE = re.compile(r"\s{2,}")


def pages_of(pdf):
    txt = subprocess.run(["pdftotext", "-layout", pdf, "-"],
                          check=True, capture_output=True).stdout.decode("utf-8")
    return txt.split("\f")


def find_page(pages, anchors):
    for p in pages:
        if all(a in p for a in anchors):
            return p
    sys.exit(f"page not found for anchors: {anchors}")


def split_row(line):
    s = line.strip()
    return SPLIT_RE.split(s) if s else []


def parse_num(tok):
    """'x' -> NA (not on ballot / not applicable); '-' -> 0; else int."""
    if tok == "x":
        return None
    if tok == "-":
        return 0
    return int(tok.replace(" ", ""))


def zero(v):
    return 0 if v is None else v


TURNOUT_LABELS = {
    "Wahlberechtigte insgesamt": "eligible",
    "Wähler insgesamt / Wahlbeteiligung": "voters",
    "Wähler/-innen insgesamt / Wahlbeteiligung": "voters",
    "Ungültige Stimmen": "invalid",
    "Ungültige Stimmzettel": "invalid",
    "Gültige Stimmen": "valid",   # 2003/2007 only (1-vote system)
}

MARKER_MAP = {
    "L": "L", "Listenstimmen": "L",
    "P": "P", "Personenstimmen": "P", "Personenstimmen**": "P",
    "Z": "Z", "Zusammen": "Z", "Insgesamt**": "Z",
}

# ---------------------------------------------------------------------------
# 2003 / 2007: one list vote -> 1 row per party, 6 values
#   [name, AnzahlBremen, %Bremen, AnzahlBHV, %BHV, AnzahlLand, %Land]
# ---------------------------------------------------------------------------
def parse_simple_year(pdf, anchors, year):
    pages = pages_of(pdf)
    p = find_page(pages, anchors)
    turnout, parties = {}, []
    started, in_party = False, False
    for l in p.split("\n"):
        parts = split_row(l)
        if parts and parts[0] in TURNOUT_LABELS and parts[0] != "Gültige Stimmen":
            started = True
            if len(parts) != 7:
                sys.exit(f"{year}: unexpected turnout row shape {parts}")
            turnout[TURNOUT_LABELS[parts[0]]] = {
                "Bremen": parse_num(parts[1]), "BHV": parse_num(parts[3]),
                "Land": parse_num(parts[5])}
            continue
        if parts and parts[0] == "Gültige Stimmen":
            started = True
            if len(parts) != 7:
                sys.exit(f"{year}: unexpected valid-votes row shape {parts}")
            turnout["valid"] = {"Bremen": parse_num(parts[1]),
                                 "BHV": parse_num(parts[3]),
                                 "Land": parse_num(parts[5])}
            continue
        if not started:
            continue
        if parts == ["Von den gültigen Stimmen entfielen auf"]:
            in_party = True
            continue
        if in_party:
            if not parts:
                break
            if len(parts) != 7:
                sys.exit(f"{year}: unexpected party row shape {parts}")
            parties.append({"name": parts[0], "Bremen": parse_num(parts[1]),
                             "BHV": parse_num(parts[3]), "Land": parse_num(parts[5])})
    if set(turnout) != {"eligible", "voters", "invalid", "valid"}:
        sys.exit(f"{year}: incomplete turnout block {turnout}")
    return {"year": year, "turnout": turnout, "parties": parties, "kind": "simple"}


# ---------------------------------------------------------------------------
# 2011 / 2023: 5-vote system -> groups of 3 rows cycling L / P / Z markers,
#   9 values each: [AnzB,%B,SitzeB, AnzBHV,%BHV,SitzeBHV, AnzL,%L,SitzeL]
# ---------------------------------------------------------------------------
def parse_5vote_year(pdf, anchors, year):
    pages = pages_of(pdf)
    p = find_page(pages, anchors)
    turnout = {}
    started = False
    section = None  # None -> turnout block; 'total'; 'party'
    cur_name_tokens, cur_rows = [], {}
    groups = []

    def flush():
        nonlocal cur_name_tokens, cur_rows, section
        if cur_rows:
            name = " ".join(cur_name_tokens).strip()
            groups.append((name, dict(cur_rows)))
            if section == "total":
                section = "party"
        cur_name_tokens, cur_rows = [], {}

    for l in p.split("\n"):
        parts = split_row(l)
        if parts and parts[0] in TURNOUT_LABELS:
            started = True
            if len(parts) != 10:
                sys.exit(f"{year}: unexpected turnout row shape {parts}")
            turnout[TURNOUT_LABELS[parts[0]]] = {
                "Bremen": parse_num(parts[1]), "BHV": parse_num(parts[4]),
                "Land": parse_num(parts[7])}
            continue
        if not started or not parts:
            continue
        if parts[0] == "Gültige Stimmen / Sitze":
            section = "total"
            continue
        if parts[0].startswith("__________") or parts[0].startswith("L: Listenstimmen"):
            flush()
            break
        if section is None:
            continue
        # find the marker token (skip a structural leading "davon" label)
        mi = None
        for i, t in enumerate(parts):
            if t == "davon":
                continue
            if t in MARKER_MAP:
                mi = i
                break
        if mi is None:
            continue  # a pure section-header line, e.g. "davon entfielen auf"
        name_tok = [t for t in parts[:mi] if t != "davon"]
        marker = MARKER_MAP[parts[mi]]
        raw9 = parts[mi + 1:mi + 10]
        if len(raw9) != 9:
            sys.exit(f"{year}: expected 9 values after marker, got {raw9} in {parts}")
        vals = {"Bremen": parse_num(raw9[0]), "BHV": parse_num(raw9[3]),
                "Land": parse_num(raw9[6])}
        if marker == "L":
            flush()
        cur_name_tokens.extend(name_tok)
        cur_rows[marker] = vals
        if marker == "Z":
            flush()

    if set(turnout) != {"eligible", "voters", "invalid"}:
        sys.exit(f"{year}: incomplete turnout block {turnout}")
    if not groups or groups[0][0] != "Insgesamt":
        sys.exit(f"{year}: expected first group to be the 'Insgesamt' total row, got {groups[:1]}")
    turnout["valid"] = groups[0][1]["Z"]
    return {"year": year, "turnout": turnout, "parties": groups[1:], "kind": "5vote"}


# ---------------------------------------------------------------------------
# uniform accessors over the two result "kinds"
# ---------------------------------------------------------------------------
def party_items(r):
    """Yield (name, {'Bremen':.., 'BHV':.., 'Land':..}) for either kind."""
    if r["kind"] == "simple":
        for p in r["parties"]:
            yield p["name"], {"Bremen": p["Bremen"], "BHV": p["BHV"], "Land": p["Land"]}
    else:
        for name, g in r["parties"]:
            yield name, g["Z"]


# ---------------------------------------------------------------------------
# VALIDATION
# ---------------------------------------------------------------------------
def validate_year(r):
    year = r["year"]
    fails = []

    def req(cond, label):
        print(("  [ok]  " if cond else "  [FAIL]") + f" {year}: {label}")
        if not cond:
            fails.append(label)

    # (1) both Wahlbereiche + Land present -- structural, always true if we got here
    req(set(r["turnout"]["valid"]) == {"Bremen", "BHV", "Land"},
        "Wahlbereich Bremen + Bremerhaven + Land Bremen all present")

    # (2) per-area: sum(parties) == valid_votes
    for area in ("Bremen", "BHV", "Land"):
        s = sum(zero(v[area]) for _, v in party_items(r))
        v = r["turnout"]["valid"][area]
        req(s == v, f"{area}: sum(party votes)={s} == valid_votes={v}")

    # (3) Bremen + BHV == Land, per party and per turnout field
    bad_parties = [name for name, v in party_items(r)
                   if zero(v["Bremen"]) + zero(v["BHV"]) != zero(v["Land"])]
    req(not bad_parties, f"every party: Bremen+BHV==Land ({len(bad_parties)} failures)")
    for key in ("eligible", "voters", "invalid", "valid"):
        t = r["turnout"][key]
        req(zero(t["Bremen"]) + zero(t["BHV"]) == zero(t["Land"]),
            f"turnout {key}: Bremen+BHV==Land")

    # (4) pinned official statewide (Land) shares, +-0.15pp
    valid_land = r["turnout"]["valid"]["Land"]
    byname = {name: v["Land"] for name, v in party_items(r)}
    for name, pin in PINNED[year].items():
        got = 100.0 * zero(byname.get(name)) / valid_land
        diff = abs(got - pin)
        req(diff <= PIN_TOL, f"pinned share {name}: got {got:.2f}% vs {pin}% (diff {diff:.3f}pp)")

    return fails


def validate_2023_js(r2023):
    """(5) cross-check 2023 shares against the InstantAtlas .js file."""
    js_path = os.path.join(RAW_DIR, "HB_2023_Buergerschaftswahl_Wahlkreis_Ortsteile_data.js")
    jd = json.loads(open(js_path, encoding="utf-8-sig").read())
    stadtbezirke = next(g for g in jd["geographies"] if g["id"] == "Stadtbezirke")
    t4 = next(t for t in stadtbezirke["themes"] if t["id"] == "t4")

    js_name_map = {
        "CDU": "CDU", "SPD": "SPD", "GRÜNE": "GRÜNE", "DIE LINKE": "DIE LINKE",
        "FDP": "FDP", "BIW": "BIW", "Die Partei": "Die PARTEI", "PIRATEN": "PIRATEN",
        "dieBasis": "dieBasis", "GFA": "GFA", "MLPD": "MLPD", "MERA25": "MERA25",
        "ÖDP": "ÖDP",
        "Partei für schulmedizinische Verjüngungsforschung":
            "Partei für schulmedizinische Verjüngungsforschung",
        "Tierschutzpartei": "Tierschutzpartei", "Volt": "Volt",
    }
    byname = dict(party_items(r2023))
    valid = r2023["turnout"]["valid"]

    checked, bad, unmapped = 0, [], []
    js_labels_seen = set()
    for ind in t4["indicators"]:
        # "<Partei> 2023 (%)", optionally suffixed "(nur Stadt Bremen/Bremerhaven)"
        m = re.match(r"^(.+?) 2023 \(%\)(?: \(nur Stadt \S+\))?$", ind["name"])
        if not m:
            continue
        js_label = m.group(1)
        js_labels_seen.add(js_label)
        if js_label not in js_name_map:
            unmapped.append(js_label)
            continue
        our_label = js_name_map[js_label]
        cv = ind["comparisonValues"]  # [Bremen, BHV, Land]
        for area, idx in (("Bremen", 0), ("BHV", 1), ("Land", 2)):
            js_val = cv[idx]
            if js_val == "" or js_val is None:
                continue
            our_pct = 100.0 * zero(byname[our_label][area]) / valid[area]
            diff = abs(our_pct - float(js_val))
            checked += 1
            if diff > 0.1:
                bad.append((our_label, area, our_pct, js_val, diff))

    our_parties_matched = set(js_name_map.values()) & set(byname)
    print(f"  [{'ok' if not unmapped else 'FAIL'}]  2023: JS party-label coverage "
          f"{len(js_name_map) - len(unmapped)}/{len(js_labels_seen)} JS indicators mapped, "
          f"{len(unmapped)} unmapped: {unmapped}")
    print(f"  [{'ok' if not bad else 'FAIL'}]  2023: {checked} (party,Wahlbereich) "
          f"cells matched InstantAtlas .js shares within 0.1pp "
          f"({len(our_parties_matched)}/{len(byname)} of our own parties covered by JS)")
    for b in bad[:10]:
        print("           ", b)
    return unmapped or bad


# ---------------------------------------------------------------------------
# main
# ---------------------------------------------------------------------------
def main():
    r2003 = parse_simple_year(
        os.path.join(RAW_DIR, "HB_2003_Buergerschaftswahl_Heft106_Bericht.pdf"),
        ["Tab. 1", "Wahl zur Bremischen Bürgerschaft", "Vorläufige Ergebnisse"], 2003)
    r2007 = parse_simple_year(
        os.path.join(RAW_DIR, "HB_2007_Buergerschaftswahl_Heft110_Bericht.pdf"),
        ["Tab. 1", "Wahl zur Bremischen Bürgerschaft (Landtag) im Lande Bremen am 13. Mai 2007",
         "Vorläufige Ergebnisse"], 2007)
    r2011 = parse_5vote_year(
        os.path.join(RAW_DIR, "HB_2011_Buergerschaftswahl_Heft113_Teil1_Analysen_Tabellen.pdf"),
        ["Tab. 1", "Wahl zur Bremischen Bürgerschaft (Landtag) im Land Bremen am 22. Mai 2011",
         "Endgültige Ergebnisse"], 2011)
    r2023 = parse_5vote_year(
        os.path.join(RAW_DIR, "HB_2023_Buergerschaftswahl_Heft126_Bericht_Ortsteile.pdf"),
        ["Tabelle 1", "Wahl zur Bremischen Bürgerschaft (Landtag) im Land Bremen am 14. Mai 2023",
         "Endgültiges Ergebnis", "Wahlberechtigte insgesamt"], 2023)

    results = [r2003, r2007, r2011, r2023]

    print("VALIDATION")
    all_fails = []
    for r in results:
        all_fails += validate_year(r)
    all_fails += list(validate_2023_js(r2023))

    if all_fails:
        print(f"\n{len(all_fails)} VALIDATION FAILURE(S) - nothing written.")
        sys.exit(1)

    # --- emit -----------------------------------------------------------------
    rows = []
    for r in results:
        year = r["year"]
        date = ELECTION_DATES[year]
        for area in ("Bremen", "BHV"):
            t = r["turnout"]
            for name, v in party_items(r):
                val = v[area]
                rows.append({
                    "state_abbr": STATE_ABBR, "state": STATE_NAME,
                    "election_year": year, "election_date": date,
                    "wkr_nr": WKR_NR[area], "wkr_name": WKR_NAME[area],
                    "stimme": STIMME,
                    "eligible_voters": t["eligible"][area],
                    "number_voters": t["voters"][area],
                    "valid_votes": t["valid"][area],
                    "invalid_votes": t["invalid"][area],
                    "party_raw": name,
                    "votes": "" if val is None else val,
                })

    os.makedirs(OUT_DIR, exist_ok=True)
    with open(OUT, "w", newline="", encoding="utf-8") as fh:
        wr = csv.DictWriter(fh, fieldnames=list(rows[0].keys()))
        wr.writeheader()
        wr.writerows(rows)

    print(f"\nWrote {len(rows)} rows -> {os.path.relpath(OUT, ROOT)}")
    for r in results:
        year = r["year"]
        n_parties = len(list(party_items(r)))
        print(f"  {year}: {n_parties} parties x 2 Wahlbereiche = "
              f"{n_parties * 2} rows ({r['kind']})")
        print(f"    party_raw values: {[name for name, _ in party_items(r)]}")


if __name__ == "__main__":
    main()
