#!/usr/bin/env python3
"""
Stage 0 — Mecklenburg-Vorpommern Bürgermeisterwahlen of the AMTSFREIE GEMEINDEN
in the Landkreis Ludwigslust-Parchim.

Why this exists
---------------
The LAIV-MV publishes Direktwahl results centrally only for the kreisfreien and
großen kreisangehörigen Städte and for the Landräte — that is what
00_mv_parse.py reads (69 PDFs -> mv_parsed.csv). The Landeswahlleitung M-V
explicitly refers researchers to the Kreiswahlleitungen for the remaining
hauptamtliche Bürgermeister, i.e. those of the amtsfreie Gemeinden.

Landkreis Ludwigslust-Parchim (Fachdienst Recht, Kommunalaufsicht und Ordnung)
supplied the results for its five amtsfreie Gemeinden on 2026-08-07 as
  "Ergebnisse BGM-Wahlen amtsfreie Gemeinden.zip"
now unpacked, verbatim, under
  data/mayoral_elections/raw/mecklenburg_vorpommern/lup_amtsfrei/

Why the results are a literal table rather than a parser
--------------------------------------------------------
The eleven delivered PDFs are one-off documents from five different Gemeinden
in at least four unrelated layouts (an amtliche Bekanntmachung, a votemanager
web print, a spreadsheet print, a scanned Wahlniederschrift). There is no shared
structure to parse, and three of them carry no usable text layer at all:

  * "Endergebnis BGM-Wahl 2014.pdf"           - scanned image, no text layer
  * "Endgültiges Ergebnis BGM-Wahl 2023.PDF"  - scanned image, no text layer
  * the three votemanager prints              - Type-3 fonts with a private
                                                encoding; pdfplumber returns
                                                "(cid:NN)" mojibake

Per the project's OCR guidance, those were read visually from 150/300-DPI
renders. The figures are therefore transcribed here as data, and every one is
re-derived from the source's own printed subtotals by the checks below — an
error in transcription fails the script rather than reaching the pipeline.

Output
------
  data/mayoral_elections/raw/mecklenburg_vorpommern/mv_lup_parsed.csv

Same candidate-level long schema as mv_parsed.csv, plus one extra column:

  is_winner                     Whether this candidate was declared elected by
                                the Gemeindewahlausschuss. Given by the SOURCE,
                                not derived from rank. 01b honours it and falls
                                back to rank where a source states none.
  flag_decisive_round_missing   The round that decided this cycle is not in the
                                source, so the cycle has NO winner (see below).
                                Currently FALSE for every row — the one gap it
                                was built for has since been filled — but keep
                                it: partial deliveries are the norm when data
                                comes from individual Kreiswahlleitungen.

Why flag_decisive_round_missing exists
--------------------------------------
The first delivery was missing Hagenow's Stichwahl of 2015-06-14, leaving only
an inconclusive Hauptwahl (Speßhardt/CDU on 41.46 %, short of the absolute
majority § 68 LKWG M-V requires). Setting is_winner = FALSE on all three
candidates was NOT enough to express that: the global winner-repair step in 01b
treats "no candidate flagged" as "the source did not say" and recrowns the
top-voted candidate — which put the CDU candidate who went on to LOSE the runoff
into mayor_panel as Hagenow's mayor from 2015.

The flag is the explicit signal that separates "nobody was elected in the rounds
we hold" from "the source is silent", and that step yields is_winner = NA for
such a cycle rather than inventing a mayor. The Landkreis supplied the missing
runoff on request (2026-08-10) — Möller/Die Linke won it 57.83 %, i.e. the
suppressed guess would indeed have been wrong — so no row is flagged today. The
mechanism stays for the next incomplete delivery.

Run:  python3 code/mayoral_elections/00_mv_lup_parse.py
"""

import csv
import datetime as dt
import os
import sys

HERE = os.path.dirname(os.path.abspath(__file__))
ROOT = os.path.abspath(os.path.join(HERE, "..", ".."))
RAW = os.path.join(ROOT, "data", "mayoral_elections", "raw",
                   "mecklenburg_vorpommern")
SRC_DIR = os.path.join(RAW, "lup_amtsfrei")
OUT_CSV = os.path.join(RAW, "mv_lup_parsed.csv")

STATE = "13"
STATE_NAME = "Mecklenburg-Vorpommern"

# ---------------------------------------------------------------------------
# AGS. Verified against data/crosswalks/final/ags_crosswalks.csv: all five are
# stable 2011-2020 and map 1:1 onto themselves in the 2021 boundaries, so every
# election below (2014-2023) sits on its own final code.
# ---------------------------------------------------------------------------
AGS = {
    "Boizenburg/Elbe": ("13076014", "Boizenburg/ Elbe, Stadt"),
    "Hagenow":         ("13076060", "Hagenow, Stadt"),
    "Lübtheen":        ("13076088", "Lübtheen, Stadt"),
    "Ludwigslust":     ("13076090", "Ludwigslust, Stadt"),
    "Parchim":         ("13076108", "Parchim, Stadt"),
}

# ---------------------------------------------------------------------------
# The results.
#
# Every unit is an amtsfreie Gemeinde, so election_type is Bürgermeisterwahl
# throughout — Ludwigslust-Parchim has no kreisfreie Stadt, hence no OB, and the
# Landratswahlen of the Kreis itself already come from the LAIV-MV PDFs.
#
# Fields per round:
#   eligible/voters/valid/invalid  as printed
#   turnout_pct                    as printed, for cross-checking (not published)
#   nein                           Ja/Nein confirmation ballot: the Nein count
#   candidates                     (last, first, party, votes, share_pct, elected)
#     share_pct is the source's own printed percentage where one is printed and
#     None where the source prints only counts; it is cross-checked, never used.
# ---------------------------------------------------------------------------
ELECTIONS = [
    # -- Boizenburg/Elbe ----------------------------------------------------
    {
        "unit": "Boizenburg/Elbe", "date": "2014-10-12", "round": "hauptwahl",
        "source": "Stadt Boizenburg/Endergebnis BGM-Wahl 2014.pdf",
        "eligible": 9240, "voters": 3082, "valid": 3056, "invalid": 26,
        "turnout_pct": None,
        "candidates": [
            ("Behrens", "Karl-Heinz",  "Einzelbewerber Behrens",  193, None, False),
            ("Jäschke", "Harald Kurt", "Einzelbewerber Jäschke", 2435, None, True),
            ("Knaak",   "Rainer",      "Einzelbewerber Knaak",    428, None, False),
        ],
        # Party labels and the candidates' order are corroborated by the
        # accompanying "zugelassene Wahlvorschläge 2014.pdf" (all three ran as
        # Einzelbewerber). Jäschke took 79.7 % — decided in the Hauptwahl.
        "notes": "endgueltiges_ergebnis",
    },
    {
        "unit": "Boizenburg/Elbe", "date": "2021-09-26", "round": "hauptwahl",
        "source": "Stadt Boizenburg/Ergebnis Bürgermeisterwahl 2021.pdf",
        "eligible": 9483, "voters": 5933, "valid": 5846, "invalid": 87,
        "turnout_pct": 62.56,
        "candidates": [
            ("Sevecke",  "Patrick", "CDU",            2669, 45.66, False),
            ("Jakobeit", "Rico",    "SPD, DIE LINKE", 2736, 46.80, False),
            ("Jülich",   "",        "Einzelbewerber",  141,  2.41, False),
            ("Pfingst",  "",        "Einzelbewerber",  300,  5.13, False),
        ],
        # No absolute majority -> Stichwahl on 2021-10-10 (below), which is what
        # seats the mayor; no candidate is elected in this round.
        # The votemanager print abbreviates Jakobeit's Wahlvorschlag to "SPD";
        # the Gemeindewahlleiter's "Bekanntmachung Stichwahlkandidaten 2021.pdf"
        # gives it in full as "SPD-Partei, DIE LINKE-Partei", which is what we
        # record (MV coalition labels elsewhere in GERDA are likewise full).
        # Jülich and Pfingst are printed by surname only — no first name exists
        # in any delivered document for them.
        "notes": "stichwahl_folgt",
    },
    {
        "unit": "Boizenburg/Elbe", "date": "2021-10-10", "round": "stichwahl",
        "source": "Stadt Boizenburg/Ergebnis Bürgermeister Stichwahl 2021.pdf",
        # "Wähler/innen" falls in the page break of the two-page print and is
        # not readable. It is fixed twice over: valid + invalid = 4322 + 9, and
        # independently the printed turnout 45.81 % x 9454 -> 4331.
        "eligible": 9454, "voters": 4331, "valid": 4322, "invalid": 9,
        "turnout_pct": 45.81,
        "candidates": [
            ("Sevecke",  "Patrick", "CDU",            1821, 42.13, False),
            ("Jakobeit", "Rico",    "SPD, DIE LINKE", 2501, 57.87, True),
        ],
        "notes": "gewaehlt_jakobeit",
    },

    # -- Hagenow ------------------------------------------------------------
    {
        "unit": "Hagenow", "date": "2015-05-31", "round": "hauptwahl",
        "source": "Stadt Hagenow/Ergebnis BGM-Wahl 2015.pdf",
        "eligible": 9847, "voters": 4416, "valid": 4375, "invalid": 41,
        "turnout_pct": 44.85,
        "candidates": [
            ("Speßhardt", "Dietmar", "CDU",       1814, 41.46, False),
            ("Möller",    "Thomas",  "Die Linke", 1653, 37.78, False),
            ("Schweda",   "Heike",   "SPD",        908, 20.75, False),
        ],
        # INCONCLUSIVE: the leader is 8.5 pp short of the absolute majority, so
        # the Stichwahl below decided this cycle; nobody is elected in this round.
        "notes": "inconclusive_hauptwahl",
    },
    {
        "unit": "Hagenow", "date": "2015-06-14", "round": "stichwahl",
        "source": "Stadt Hagenow/Ergebnis BGM-Stichwahl 2015.pdf",
        "eligible": 9824, "voters": 4176, "valid": 4159, "invalid": 17,
        "turnout_pct": 42.51,
        "candidates": [
            ("Speßhardt", "Dietmar", "CDU",       1754, 42.17, False),
            ("Möller",    "Thomas",  "Die Linke", 2405, 57.83, True),
        ],
        # Supplied 2026-08-10 after we asked: it had been dropped when the
        # original zip was built. Both names are spelled exactly as in the
        # Hauptwahl, so the HW/SW pairing in 01b keys cleanly.
        # Wahlberechtigte legitimately differ between the rounds (9847 -> 9824);
        # the Wählerverzeichnis was updated between 31.05. and 14.06.
        "notes": "vorlaeufiges_endergebnis",
    },
    {
        "unit": "Hagenow", "date": "2022-05-08", "round": "hauptwahl",
        "source": "Stadt Hagenow/vorl. Ergebnis BGM-Wahl 2022.pdf",
        "eligible": 10018, "voters": 3613, "valid": 3556, "invalid": 57,
        "turnout_pct": 36.07,
        "candidates": [
            ("Horn",   "Jana",   "CDU",       1616, 45.44, False),
            ("Möller", "Thomas", "DIE LINKE", 1940, 54.56, True),
        ],
        # The only delivered document for this election is the "vorläufiges
        # Endergebnis" after all 13 of 13 Wahllokale; it is complete and
        # internally exact, but it is not the amtliche Feststellung.
        "notes": "vorlaeufiges_endergebnis",
    },

    # -- Lübtheen -----------------------------------------------------------
    {
        "unit": "Lübtheen", "date": "2022-09-11", "round": "hauptwahl",
        "source": "Stadt Lübtheen/Ergebnis Bürgermeisterwahl 2022.pdf",
        # Sole candidate -> Ja/Nein confirmation ballot: gültige = Ja + Nein.
        # "Wahlberechtigte" falls in the page break of the two-page print; it is
        # recovered from the printed turnout, which pins it uniquely:
        # 1297 / 0.318 45..0.318 55 = (4071.6, 4072.9] -> 4072.
        "eligible": 4072, "voters": 1297, "valid": 1287, "invalid": 10,
        "turnout_pct": 31.85, "nein": 124,
        "candidates": [
            ("Lindenau", "Ute Annegret", "SPD", 1163, 90.37, True),
        ],
        "notes": "ja_nein_wahl;eligible_voters_aus_wahlbeteiligung_abgeleitet",
    },

    # -- Ludwigslust --------------------------------------------------------
    {
        "unit": "Ludwigslust", "date": "2023-11-26", "round": "hauptwahl",
        "source": "Stadt Ludwigslust/Endgültiges Ergebnis BGM-Wahl 2023.PDF",
        "eligible": 9947, "voters": 4134, "valid": 3896, "invalid": 238,
        "turnout_pct": 41.56,
        "candidates": [
            ("Pinnow",        "Stefan", "SPD",            2035, 52.23, True),
            ("Schwarzenberg", "Maik",   "Einzelbewerber", 1584, 40.66, False),
            ("Klein",         "Tommy",  "Grüne",           277,  7.11, False),
        ],
        # Page 2 of the same PDF is the Wahlbezirk-level Einzelblatt zu Anlage
        # 22.2; its GemSumme row reproduces all of the above exactly.
        "notes": "endgueltiges_ergebnis",
    },

    # -- Parchim ------------------------------------------------------------
    {
        "unit": "Parchim", "date": "2022-04-24", "round": "hauptwahl",
        "source": "Stadt Parchim/endgültiges Ergebnis BGM-Wahl 2022.pdf",
        "eligible": 14384, "voters": 5780, "valid": 5714, "invalid": 66,
        "turnout_pct": 40.18,
        "candidates": [
            ("Langer", "Sebastian", "", 2278, None, False),
            ("Flörke", "Dirk",      "", 3436, None, True),
        ],
        # The Bekanntmachung names the Bewerber and their vote counts but no
        # Wahlvorschlagsträger at all, so candidate_party stays empty for both.
        # It is left empty rather than filled from an outside source: the party
        # column of GERDA records the formal Wahlvorschlag, which this document
        # does not state. Langer is printed with the title "Dr."; the title is
        # kept in candidate_name and out of candidate_last_name so that the
        # HW/SW name pairing in 01b keys on the bare surname.
        "notes": "endgueltiges_ergebnis;kein_wahlvorschlagstraeger_in_quelle",
    },
]

TITLES = {("Langer", "Sebastian"): "Dr."}

OUT_COLS = ["ags", "ags_name", "state", "state_name", "election_year",
            "election_date", "election_type", "round", "eligible_voters",
            "number_voters", "valid_votes", "invalid_votes", "turnout",
            "candidate_name", "candidate_last_name", "candidate_first_name",
            "candidate_party", "candidate_votes", "candidate_voteshare",
            "ja_nein", "is_winner", "flag_decisive_round_missing",
            "winner_name_raw", "source_file", "notes"]


def fail(msg):
    print("FAIL: " + msg, file=sys.stderr)
    fail.n += 1


fail.n = 0


def check(e):
    """Re-derive every published number from the source's own subtotals."""
    tag = "%s %s %s" % (e["unit"], e["date"], e["round"])

    if e["unit"] not in AGS:
        fail("%s: unknown Gemeinde" % tag)
        return

    d = dt.date.fromisoformat(e["date"])
    if d.weekday() != 6:
        fail("%s: election date is a %s, not a Sunday"
             % (tag, d.strftime("%A")))

    path = os.path.join(SRC_DIR, e["source"])
    if not os.path.exists(path):
        fail("%s: source file missing: %s" % (tag, e["source"]))

    ev, nv = e["eligible"], e["voters"]
    valid, invalid = e["valid"], e["invalid"]

    # 1. Ballots cast decompose exactly into valid + invalid.
    if valid + invalid != nv:
        fail("%s: valid+invalid=%d != voters=%d" % (tag, valid + invalid, nv))

    # 2. Turnout matches the printed percentage (2 dp).
    if e.get("turnout_pct") is not None:
        got = round(100.0 * nv / ev, 2)
        if abs(got - e["turnout_pct"]) > 0.005:
            fail("%s: turnout %.2f%% != printed %.2f%%"
                 % (tag, got, e["turnout_pct"]))

    # 3. Candidate votes account for every valid vote. On a Ja/Nein ballot the
    #    Nein votes are valid too, so they must be added in.
    tot = sum(c[3] for c in e["candidates"]) + e.get("nein", 0)
    if tot != valid:
        fail("%s: candidate votes %d != valid %d" % (tag, tot, valid))

    if "nein" in e and len(e["candidates"]) != 1:
        fail("%s: Ja/Nein ballot with %d candidates"
             % (tag, len(e["candidates"])))

    # 4. Each printed candidate percentage matches votes / valid.
    for last, first, _party, votes, share_pct, _won in e["candidates"]:
        if share_pct is None:
            continue
        got = round(100.0 * votes / valid, 2)
        if abs(got - share_pct) > 0.011:
            fail("%s: %s share %.2f%% != printed %.2f%%"
                 % (tag, last, got, share_pct))

    # 5. At most one candidate is elected, and only on an absolute majority
    #    (§ 68 LKWG M-V) — the guard against seating the leader of an
    #    inconclusive round or a rejected Ja/Nein candidate.
    won = [c for c in e["candidates"] if c[5]]
    if len(won) > 1:
        fail("%s: %d candidates marked elected" % (tag, len(won)))
    for c in won:
        if c[3] / valid <= 0.5:
            fail("%s: %s marked elected on %.2f%% of valid votes"
                 % (tag, c[0], 100.0 * c[3] / valid))

    # 6. Names must be usable as a pairing key.
    for last, first, _p, _v, _s, _w in e["candidates"]:
        if not last.strip():
            fail("%s: candidate without a surname" % tag)


def check_cycles():
    """Every cycle either seats exactly one mayor or is flagged as incomplete.

    A cycle is one (Gemeinde, year): a Hauptwahl plus, where there was one, its
    Stichwahl. This is the invariant that keeps the two signals from drifting
    apart — a flagged cycle must not name a winner, and an unflagged cycle must,
    which is what stops a silently truncated delivery from being published as a
    decided election.
    """
    cycles = {}
    for e in ELECTIONS:
        cycles.setdefault((e["unit"], e["date"][:4]), []).append(e)

    for (unit, year), rounds in sorted(cycles.items()):
        tag = "%s %s" % (unit, year)
        won = [c for r in rounds for c in r["candidates"] if c[5]]
        flagged = {bool(r.get("decisive_round_missing")) for r in rounds}

        if len(flagged) > 1:
            fail("%s: decisive_round_missing set on some rounds but not all"
                 % tag)
        incomplete = True in flagged

        if incomplete and won:
            fail("%s: flagged incomplete yet marks %s elected"
                 % (tag, won[0][0]))
        elif not incomplete and len(won) != 1:
            fail("%s: %d candidates elected across %d round(s); a complete "
                 "cycle seats exactly one (flag it if the deciding round is "
                 "not in the source)" % (tag, len(won), len(rounds)))

        # A Stichwahl only exists because its Hauptwahl was inconclusive.
        by_round = {r["round"] for r in rounds}
        if "stichwahl" in by_round and "hauptwahl" not in by_round:
            fail("%s: Stichwahl without its Hauptwahl" % tag)
        for r in rounds:
            if r["round"] != "hauptwahl":
                continue
            top = max(c[3] for c in r["candidates"]) / r["valid"]
            decided = top > 0.5
            if decided and "stichwahl" in by_round:
                fail("%s: Hauptwahl won outright (%.1f%%) yet a Stichwahl "
                     "follows" % (tag, 100 * top))
            if not decided and "stichwahl" not in by_round and not incomplete:
                fail("%s: Hauptwahl inconclusive (top %.1f%%) with no "
                     "Stichwahl and no incomplete flag" % (tag, 100 * top))


def main():
    for e in ELECTIONS:
        check(e)
    check_cycles()
    if fail.n:
        sys.exit("aborting: %d validation failure(s)" % fail.n)

    rows = []
    for e in ELECTIONS:
        ags, ags_name = AGS[e["unit"]]
        won = [c for c in e["candidates"] if c[5]]
        winner_raw = ""
        if won:
            last, first = won[0][0], won[0][1]
            winner_raw = ("%s, %s" % (last, first)) if first else last
        turnout = e["voters"] / e["eligible"]
        for last, first, party, votes, _share_pct, elected in e["candidates"]:
            title = TITLES.get((last, first), "")
            shown = ("%s %s" % (title, last)).strip()
            name = ("%s, %s" % (shown, first)) if first else shown
            rows.append({
                "ags": ags,
                "ags_name": ags_name,
                "state": STATE,
                "state_name": STATE_NAME,
                "election_year": dt.date.fromisoformat(e["date"]).year,
                "election_date": e["date"],
                "election_type": "Bürgermeisterwahl",
                "round": e["round"],
                "eligible_voters": e["eligible"],
                "number_voters": e["voters"],
                "valid_votes": e["valid"],
                "invalid_votes": e["invalid"],
                "turnout": turnout,
                "candidate_name": name,
                "candidate_last_name": last,
                "candidate_first_name": first,
                "candidate_party": party,
                "candidate_votes": votes,
                "candidate_voteshare": votes / e["valid"],
                "ja_nein": int("nein" in e),
                "is_winner": "TRUE" if elected else "FALSE",
                "flag_decisive_round_missing":
                    "TRUE" if e.get("decisive_round_missing") else "FALSE",
                "winner_name_raw": winner_raw,
                "source_file": e["source"],
                "notes": e["notes"],
            })

    with open(OUT_CSV, "w", newline="", encoding="utf-8") as f:
        w = csv.DictWriter(f, fieldnames=OUT_COLS)
        w.writeheader()
        w.writerows(rows)

    units = sorted({r["ags_name"] for r in rows})
    years = sorted({r["election_year"] for r in rows})
    seated = sum(1 for r in rows if r["is_winner"] == "TRUE")
    cycles = {(r["ags"], r["election_year"]) for r in rows}
    print("Wrote %s" % os.path.relpath(OUT_CSV, ROOT))
    print("  %d candidate rows | %d rounds | %d cycles | %d Gemeinden | %d-%d"
          % (len(rows), len(ELECTIONS), len(cycles), len(units),
             years[0], years[-1]))
    print("  mayors seated: %d of %d cycles" % (seated, len(cycles)))
    for u in units:
        rs = [r for r in rows if r["ags_name"] == u]
        print("    %-24s %d rows, %s" % (
            u, len(rs), ", ".join(sorted({r["election_date"] for r in rs}))))
    nodec = sorted(cycles - {(r["ags"], r["election_year"])
                             for r in rows if r["is_winner"] == "TRUE"})
    for ags, yr in nodec:
        print("  NOTE: %s %d has no elected candidate "
              "(decisive round not in the source)" % (ags, yr))


if __name__ == "__main__":
    main()
