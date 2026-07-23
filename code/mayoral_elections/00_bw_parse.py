#!/usr/bin/env python3
"""Stage-0 parser for Baden-Württemberg mayoral (Bürgermeister) elections.

Source: Statistisches Landesamt Baden-Württemberg, Statistischer Bericht
B VII 3-j/25, "Ergebnisse der Bürgermeisterwahlen in Baden-Württemberg,
Stichtag: 31.12.2024" (Artikel-Nr. 4253 25001), a 74-page PDF.

Since a 2023 amendment to the Kommunalwahlgesetz (KomWG §39a/§45, in force
1 Aug 2023), the Statistical Office centrally collects the result of *every*
mayoral election. The report records, for each of the ~1,101 Gemeinden, the
*most recent* mayoral election as of 31.12.2024 (so election dates span
roughly 2017-2024). Two municipality-level tables carry everything we need:

  * Table 13 ("Ergebnisse der jeweils letzten Bürgermeisterwahlen") — one or
    more rounds per Gemeinde with Wahlart (H=Hauptwahl, N=Neuwahl,
    S=Stichwahl), Wahltag, Wahlgrund (A=Ablauf der Amtszeit, S=Sonstige),
    Wahlberechtigte, Wähler, Briefwähler, ungültige und gültige Stimmen.
  * Table 14 ("Gewählte Bürgermeisterinnen und Bürgermeister") — the elected
    person per Gemeinde: Name, Frau (F=female), Unionsbürgerschaft,
    Geburtsjahr, Amtsperiode, and the valid votes they received in the
    *decisive* round (Hauptwahl, or Neuwahl/Stichwahl if one was held).

Important characteristics of the BW source (documented limitations):
  * No party affiliation is collected — in BW mayoral candidates have no
    Wahlvorschlagsträger (parties). candidate_party is therefore always NA.
  * Only the *winner* is reported, not the losing candidates. The intermediate
    is therefore winner-only (one candidate row per round).
  * The winner's vote count is reported only for the decisive round. For a
    Gemeinde that needed a Neuwahl/Stichwahl, the winner's Hauptwahl votes are
    unknown → winner votes/voteshare on the (non-decisive) Hauptwahl row are NA.

Output (one row per Gemeinde-round, winner-level long, MV/TH-compatible):
  data/mayoral_elections/raw/baden_wuerttemberg/bw_parsed.csv

The CSV is **lossless** w.r.t. the report's per-Gemeinde tables: alongside the
columns the R stages consume, it carries every other field the report gives for
the elected person / round but that the unified GERDA schema does not surface —
`amtsperiode` (term number of the elected mayor), `hauptamtlich` (TRUE =
hauptamtlich / FALSE = ehrenamtlich), `eu_citizen` (Unionsbürgerschaft, i.e. a
non-German EU national), `briefwaehler` (postal-vote count) and `wahlgrund`
(A = Ablauf der Amtszeit, S = Sonstige Gründe). These BW-only extras are kept
here for anyone who wants them; the R Stage-1 scripts ignore them.

Run:  python3 code/mayoral_elections/00_bw_parse.py

The R Stage-1 scripts (01_mayoral_unharm.R, 01b_mayoral_candidates.R) read the
CSV via SH/MV-style blocks.
"""

import csv
import os
import re
import sys
import unicodedata

import pdfplumber

# --------------------------------------------------------------------------
# Paths
# --------------------------------------------------------------------------
HERE = os.path.dirname(os.path.abspath(__file__))
ROOT = os.path.dirname(os.path.dirname(HERE))
RAW_DIR = os.path.join(ROOT, "data", "mayoral_elections", "raw", "baden_wuerttemberg")
PDF = os.path.join(RAW_DIR, "BW_Buergermeisterwahlen_2024_StatBericht_B-VII-3-j25.pdf")
OUT = os.path.join(RAW_DIR, "bw_parsed.csv")

STATE = "08"
STATE_NAME = "Baden-Württemberg"

# --------------------------------------------------------------------------
# Office classification: Oberbürgermeister vs Bürgermeister
# --------------------------------------------------------------------------
# In BW the head of a Stadtkreis (9) or a Große Kreisstadt (96, Stand 1.1.2025)
# bears the title "Oberbürgermeister/-in"; every other Gemeinde has a
# "Bürgermeister/-in". The report does NOT carry the office title, so we
# classify by Gemeinde. Name matching alone is ambiguous (e.g. "Weingarten,
# Stadt" AGS 08436082 is a Große Kreisstadt, but "Weingarten (Baden)" AGS
# 08215090 is not), so the resolved Große-Kreisstadt AGS set is verified
# against the expected count (96) at parse time and the one genuine name
# collision is pinned by AGS.

# The 9 Stadtkreise (kreisfreie Städte) — pinned by 8-digit AGS.
STADTKREIS_AGS = {
    "08111000",  # Stuttgart
    "08121000",  # Heilbronn
    "08211000",  # Baden-Baden
    "08212000",  # Karlsruhe
    "08221000",  # Heidelberg
    "08222000",  # Mannheim
    "08231000",  # Pforzheim
    "08311000",  # Freiburg im Breisgau
    "08421000",  # Ulm
}

# The 96 Große Kreisstädte (Stand 1. Januar 2025), display names from the
# Wikipedia "Liste der kreisangehörigen Städte mit Sonderstatus in Deutschland"
# (a copy of the source wikitext is kept alongside the raw PDF). Matched to the
# report's Gemeinde column by normalised name; see resolve_ob_ags().
GROSSE_KREISSTAEDTE = [
    "Aalen", "Achern", "Albstadt", "Backnang", "Bad Krozingen", "Bad Mergentheim",
    "Bad Rappenau", "Bad Waldsee", "Balingen", "Biberach an der Riß",
    "Bietigheim-Bissingen", "Böblingen", "Bretten", "Bruchsal", "Bühl", "Calw",
    "Crailsheim", "Ditzingen", "Donaueschingen", "Ehingen (Donau)", "Eislingen/Fils",
    "Ellwangen (Jagst)", "Emmendingen", "Eppingen", "Esslingen am Neckar", "Ettlingen",
    "Fellbach", "Filderstadt", "Freudenstadt", "Friedrichshafen", "Gaggenau",
    "Geislingen an der Steige", "Giengen an der Brenz", "Göppingen",
    "Heidenheim an der Brenz", "Herrenberg", "Hockenheim", "Horb am Neckar", "Kehl",
    "Kirchheim unter Teck", "Konstanz", "Kornwestheim", "Lahr/Schwarzwald", "Laupheim",
    "Leimen", "Leinfelden-Echterdingen", "Leonberg", "Leutkirch im Allgäu", "Lörrach",
    "Ludwigsburg", "Metzingen", "Mosbach", "Mössingen", "Mühlacker", "Nagold",
    "Neckarsulm", "Nürtingen", "Oberkirch", "Offenburg", "Öhringen", "Ostfildern",
    "Radolfzell am Bodensee", "Rastatt", "Ravensburg", "Remseck am Neckar", "Reutlingen",
    "Rheinfelden (Baden)", "Rheinstetten", "Rottenburg am Neckar", "Rottweil",
    "Schorndorf", "Schramberg", "Schwäbisch Gmünd", "Schwäbisch Hall", "Schwetzingen",
    "Sindelfingen", "Singen (Hohentwiel)", "Sinsheim", "Stutensee", "Tübingen",
    "Tuttlingen", "Überlingen", "Vaihingen an der Enz", "Villingen-Schwenningen",
    "Waghäusel", "Waiblingen", "Waldkirch", "Waldshut-Tiengen", "Wangen im Allgäu",
    "Weil am Rhein", "Weingarten", "Weinheim", "Weinstadt", "Wertheim", "Wiesloch",
    "Winnenden",
]
N_GROSSE_KREISSTAEDTE = 96
# Pin the genuine name collision: the Große Kreisstadt "Weingarten" is AGS
# 08436082 (Lkr. Ravensburg), NOT "Weingarten (Baden)" 08215090 (Lkr. Karlsruhe).
GKS_AGS_OVERRIDE = {"weingarten": "08436082"}

# --------------------------------------------------------------------------
# Parsing is token-order based, NOT fixed-x based: the table block is centred
# slightly differently on each page (drifts ~20pt), so absolute x-boundaries
# calibrated on one page misread others. Within a row, however, the *order* of
# tokens is stable, so we anchor on unambiguous tokens (the dd.mm.yyyy Wahltag
# and the literal "Anzahl" in Table 13; the rightmost numeric block in Table 14).
# --------------------------------------------------------------------------
ROW_TOL = 6.0  # vertical tolerance (points) for grouping words into one row
DATE_RE = re.compile(r"^\d{2}\.\d{2}\.\d{4}$")
YEAR_RE = re.compile(r"^(?:18|19|20)\d{2}$")


# --------------------------------------------------------------------------
# Helpers
# --------------------------------------------------------------------------
def norm_name(s):
    """Normalise a Gemeinde name for GKS matching: drop the city-status suffix,
    lowercase, collapse whitespace (parentheticals/slashes kept on purpose)."""
    s = s.strip()
    s = re.sub(r",\s*(Landeshauptstadt|Universitätsstadt|Stadt)\s*$", "", s)
    s = s.lower()
    s = re.sub(r"\s+", " ", s).strip()
    return s


def to_int(s):
    if s is None:
        return None
    s = s.strip().replace(".", "").replace("–", "").replace("-", "")
    if s == "":
        return None
    try:
        return int(s)
    except ValueError:
        return None


def to_pct_fraction(s):
    """German percentage string (e.g. '42,3') -> fraction (0.423)."""
    if s is None:
        return None
    s = s.strip().replace("–", "").replace("%", "")
    if s == "" or s == "-":
        return None
    s = s.replace(".", "").replace(",", ".")
    try:
        return float(s) / 100.0
    except ValueError:
        return None


def cluster_rows(words):
    """Group words into visual rows by their 'top' coordinate."""
    rows = []
    for w in sorted(words, key=lambda x: (x["top"], x["x0"])):
        if rows and abs(w["top"] - rows[-1][0]) <= ROW_TOL:
            rows[-1][1].append(w)
        else:
            rows.append([w["top"], [w]])
    return [r[1] for r in rows]


def split_name(full):
    """'Dr. Nopper, Frank' -> (full, last='Nopper', first='Frank')."""
    full = re.sub(r"\s+", " ", full).strip()
    if "," in full:
        last, first = full.split(",", 1)
    else:
        last, first = full, ""
    last = last.strip()
    first = first.strip()
    # Strip leading academic titles from the surname part.
    last_clean = re.sub(
        r"^(?:(?:Prof\.|Dr\.|h\.\s*c\.|Dipl\.[-\w]*|Ing\.|jur\.|med\.|rer\.\s*\w+\.?)\s*)+",
        "", last,
    ).strip()
    return full, (last_clean or last), first


# --------------------------------------------------------------------------
# Page-type detection
# --------------------------------------------------------------------------
def page_kind(page):
    txt = (page.extract_text() or "")[:400]
    if "Ergebnisse der jeweils letzten" in txt:
        return 13
    if "Gewählte Bürgermeisterinnen" in txt:
        return 14
    return None


def data_rows(page):
    """Yield (ags6, content_words) for genuine municipality rows.

    The leftmost cell holds the Lfd.Nr and the 6-digit AGS. pdfplumber sometimes
    merges them into one token ('2413436081') and sometimes keeps them separate
    ('2413', '436081') depending on the page — so we locate the AGS as the first
    left-margin all-digit token of length >= 6 and return the words *after* it."""
    words = [w for w in page.extract_words(use_text_flow=False,
                                            keep_blank_chars=False)
             if w["top"] > 108]  # skip page title / column headers
    for row in cluster_rows(words):
        ws = sorted(row, key=lambda x: x["x0"])
        ai = next((i for i, w in enumerate(ws)
                   if w["x0"] < 100 and w["text"].isdigit() and len(w["text"]) >= 6),
                  None)
        if ai is None:
            continue
        yield ws[ai]["text"][-6:], ws[ai + 1:]


# --------------------------------------------------------------------------
# Parse Table 13 (rounds) and Table 14 (winners)
# --------------------------------------------------------------------------
def parse_table13(pdf):
    """Return dict: ags8 -> {'gemeinde':.., 'rounds':[ {round dict} ]}.

    Row layout (Anzahl rows): <ags> <gemeinde...> <H|N|S> <dd.mm.yyyy> <A|S>
    Anzahl <wahlberechtigte> <wähler> <briefwähler> <ungültige> <gültige>.
    The percentage rows (Einheit '%') carry no Wahltag and are ignored."""
    munis = {}
    malformed = []
    for page in pdf.pages:
        if page_kind(page) != 13:
            continue
        for ags6, ws in data_rows(page):
            toks = [w["text"] for w in ws]
            # Anchor on the Wahltag (only Anzahl rows have one).
            dt_idx = next((i for i, t in enumerate(toks) if DATE_RE.match(t)), None)
            if dt_idx is None or "Anzahl" not in toks:
                continue
            wahlart = toks[dt_idx - 1]
            if wahlart not in ("H", "N", "S"):
                malformed.append((ags6, toks))
                continue
            an_idx = toks.index("Anzahl")
            nums = toks[an_idx + 1:an_idx + 6]   # exactly 5 count columns
            if len(nums) < 5:
                malformed.append((ags6, toks))
                continue
            gemeinde = " ".join(toks[:dt_idx - 1]).strip()
            wahlgrund = toks[dt_idx + 1] if dt_idx + 1 < an_idx else ""
            ags8 = STATE + ags6
            rec = munis.setdefault(ags8, {"gemeinde": gemeinde, "rounds": []})
            rec["rounds"].append({
                "wahlart": wahlart,
                "round": "hauptwahl" if wahlart == "H" else "stichwahl",
                "wahltag": toks[dt_idx],
                "wahlgrund": wahlgrund,
                "eligible_voters": to_int(nums[0]),
                "number_voters": to_int(nums[1]),
                "briefwaehler": to_int(nums[2]),
                "invalid_votes": to_int(nums[3]),
                "valid_votes": to_int(nums[4]),
            })
    if malformed:
        sys.stderr.write(f"  NOTE: {len(malformed)} Table-13 rows skipped as malformed\n")
        for a, t in malformed[:8]:
            sys.stderr.write(f"     {a}: {t}\n")
    return munis


def parse_table14(pdf):
    """Return dict: ags8 -> winner dict.

    Row layout: <ags> <gemeinde...> <name...> [H] [F] [EU] <geburtsjahr>
    <amtsperiode> <gültige Stimmen Anzahl> <%>. We anchor on the rightmost
    4-digit Geburtsjahr; the three trailing fields are amtsperiode/votes/%, and
    the single-letter tokens between the name and the Geburtsjahr are the
    Hauptamt (H) / Frau (F) / Unionsbürger markers."""
    winners = {}
    malformed = []
    for page in pdf.pages:
        if page_kind(page) != 14:
            continue
        for ags6, ws in data_rows(page):
            toks = [w["text"] for w in ws]
            # Geburtsjahr = 4-digit year token with exactly 3 fields to its right.
            g = next((i for i in range(len(toks) - 3)
                      if YEAR_RE.match(toks[i])
                      and to_int(toks[i + 1]) is not None
                      and to_int(toks[i + 2]) is not None
                      and to_pct_fraction(toks[i + 3]) is not None
                      and i + 4 == len(toks)), None)
            if g is None or g < 2:
                malformed.append((ags6, toks))
                continue
            # Middle block = Gemeinde + Name + single-letter markers (H/F/EU).
            mid = ws[:g]
            markers = []
            while mid and re.fullmatch(r"[A-ZÄÖÜ]", mid[-1]["text"]):
                markers.append(mid.pop()["text"])
            # Gemeinde and Name sit in separate columns with a wide x-gap between
            # them; within either, word gaps are small. Split at the largest gap.
            if len(mid) < 2:
                name = " ".join(w["text"] for w in mid).strip()
            else:
                gaps = [(mid[i + 1]["x0"] - mid[i]["x1"], i) for i in range(len(mid) - 1)]
                _, cut = max(gaps)
                name = " ".join(w["text"] for w in mid[cut + 1:]).strip()
            if not name:
                malformed.append((ags6, toks))
                continue
            full, last, first = split_name(name)
            winners[STATE + ags6] = {
                "name": full,
                "last_name": last,
                "first_name": first,
                "gender": "female" if "F" in markers else "male",
                "hauptamtlich": "H" in markers,
                "eu_citizen": any(m not in ("H", "F") for m in markers),
                "birth_year": to_int(toks[g]),
                "amtsperiode": to_int(toks[g + 1]),
                "votes": to_int(toks[g + 2]),
                "voteshare": to_pct_fraction(toks[g + 3]),
            }
    if malformed:
        sys.stderr.write(f"  NOTE: {len(malformed)} Table-14 rows skipped as malformed\n")
        for a, t in malformed[:8]:
            sys.stderr.write(f"     {a}: {t}\n")
    return winners


# --------------------------------------------------------------------------
# Office classification
# --------------------------------------------------------------------------
def resolve_ob_ags(munis):
    """Resolve the set of AGS whose head is an Oberbürgermeister (Stadtkreise +
    Große Kreisstädte). Verifies the GKS match count and pins the one collision."""
    by_norm = {}
    for ags8, rec in munis.items():
        by_norm.setdefault(norm_name(rec["gemeinde"]), []).append(ags8)

    ob = set(STADTKREIS_AGS)
    matched, unmatched = 0, []
    for gks in GROSSE_KREISSTAEDTE:
        key = norm_name(gks)
        if key in GKS_AGS_OVERRIDE:
            ob.add(GKS_AGS_OVERRIDE[key])
            matched += 1
            continue
        cands = by_norm.get(key, [])
        if len(cands) == 1:
            ob.add(cands[0])
            matched += 1
        elif len(cands) == 0:
            unmatched.append((gks, "no match"))
        else:
            unmatched.append((gks, f"ambiguous: {cands}"))

    if unmatched:
        sys.stderr.write("ERROR: unresolved Große Kreisstädte:\n")
        for g, why in unmatched:
            sys.stderr.write(f"   - {g}: {why}\n")
    if matched != N_GROSSE_KREISSTAEDTE:
        raise SystemExit(
            f"FATAL: matched {matched} Große Kreisstädte, expected "
            f"{N_GROSSE_KREISSTAEDTE}. OB classification would be wrong.")
    # Confirm the pinned Stadtkreise are actually present in the data.
    missing_sk = [a for a in STADTKREIS_AGS if a not in munis]
    if missing_sk:
        raise SystemExit(f"FATAL: Stadtkreis AGS missing from report: {missing_sk}")
    return ob


# --------------------------------------------------------------------------
# Build the intermediate
# --------------------------------------------------------------------------
FIELDS = [
    "ags", "ags_name", "state", "state_name", "election_year", "election_date",
    "election_type", "round", "eligible_voters", "number_voters", "valid_votes",
    "invalid_votes", "turnout", "candidate_name", "candidate_last_name",
    "candidate_first_name", "candidate_party", "candidate_votes",
    "candidate_voteshare", "candidate_gender", "candidate_birth_year",
    "is_decisive", "briefwaehler", "wahlgrund", "amtsperiode",
    "hauptamtlich", "eu_citizen", "source_file",
]


def iso_date(de):
    """'08.11.2020' -> '2020-11-08'."""
    m = re.match(r"(\d{2})\.(\d{2})\.(\d{4})", de or "")
    return f"{m.group(3)}-{m.group(2)}-{m.group(1)}" if m else ""


def build_rows(munis, winners, ob_ags):
    rows = []
    n_no_winner = 0
    bad_share = []
    for ags8, rec in munis.items():
        rnds = rec["rounds"]
        # Decisive round = the chronologically last one (Neuwahl/Stichwahl if any).
        rnds_sorted = sorted(rnds, key=lambda r: iso_date(r["wahltag"]))
        decisive = rnds_sorted[-1]
        win = winners.get(ags8)
        if win is None:
            n_no_winner += 1
        etype = ("Oberbürgermeisterwahl" if ags8 in ob_ags
                 else "Bürgermeisterwahl")

        # Validate the join: winner votes / decisive valid votes ~= reported share.
        if win and win["votes"] and decisive["valid_votes"] and win["voteshare"]:
            calc = win["votes"] / decisive["valid_votes"]
            if abs(calc - win["voteshare"]) > 0.003:
                bad_share.append((ags8, rec["gemeinde"], round(calc, 4),
                                  win["voteshare"]))

        for rnd in rnds_sorted:
            is_dec = rnd is decisive
            ev, nv = rnd["eligible_voters"], rnd["number_voters"]
            turnout = (nv / ev) if (ev and nv) else ""
            rows.append({
                "ags": ags8,
                "ags_name": re.sub(r",\s*(Landeshauptstadt|Universitätsstadt|Stadt)\s*$",
                                   "", rec["gemeinde"]).strip(),
                "state": STATE,
                "state_name": STATE_NAME,
                "election_year": iso_date(rnd["wahltag"])[:4],
                "election_date": iso_date(rnd["wahltag"]),
                "election_type": etype,
                "round": rnd["round"],
                "eligible_voters": ev if ev is not None else "",
                "number_voters": nv if nv is not None else "",
                "valid_votes": rnd["valid_votes"] if rnd["valid_votes"] is not None else "",
                "invalid_votes": rnd["invalid_votes"] if rnd["invalid_votes"] is not None else "",
                "turnout": turnout,
                # Winner identity is attached to every round (so the Hauptwahl
                # shell row pairs with the Stichwahl row by name in 01b), but
                # the winner's votes/voteshare are known only for the decisive round.
                "candidate_name": (win["name"] if win else ""),
                "candidate_last_name": (win["last_name"] if win else ""),
                "candidate_first_name": (win["first_name"] if win else ""),
                "candidate_party": "",  # BW collects no party affiliation
                "candidate_votes": (win["votes"] if (win and is_dec) else ""),
                "candidate_voteshare": (win["voteshare"] if (win and is_dec) else ""),
                "candidate_gender": (win["gender"] if win else ""),
                "candidate_birth_year": (win["birth_year"] if win else ""),
                "is_decisive": "TRUE" if is_dec else "FALSE",
                "briefwaehler": rnd["briefwaehler"] if rnd["briefwaehler"] is not None else "",
                "wahlgrund": rnd["wahlgrund"],
                "amtsperiode": (win["amtsperiode"] if win else ""),
                # Winner attributes from Table 14, captured so the raw intermediate
                # is lossless. Not surfaced in the unified GERDA schema (BW-only):
                "hauptamtlich": ("TRUE" if win["hauptamtlich"] else "FALSE") if win else "",
                "eu_citizen": ("TRUE" if win["eu_citizen"] else "FALSE") if win else "",
                "source_file": os.path.basename(PDF),
            })
    return rows, n_no_winner, bad_share


def main():
    if not os.path.exists(PDF):
        raise SystemExit(f"Raw PDF not found: {PDF}")
    with pdfplumber.open(PDF) as pdf:
        munis = parse_table13(pdf)
        winners = parse_table14(pdf)

    ob_ags = resolve_ob_ags(munis)
    rows, n_no_winner, bad_share = build_rows(munis, winners, ob_ags)

    # ---- diagnostics -----------------------------------------------------
    n_munis = len(munis)
    n_rounds = sum(len(r["rounds"]) for r in munis.values())
    n_h = sum(1 for r in munis.values() for x in r["rounds"] if x["wahlart"] == "H")
    n_n = sum(1 for r in munis.values() for x in r["rounds"] if x["wahlart"] == "N")
    n_s = sum(1 for r in munis.values() for x in r["rounds"] if x["wahlart"] == "S")
    n_ob = sum(1 for a in munis if a in ob_ags)

    sys.stderr.write(
        f"\n=== Baden-Württemberg Bürgermeisterwahlen parse ===\n"
        f"  Gemeinden (munis):        {n_munis}\n"
        f"  Round-rows total:         {n_rounds}\n"
        f"    Hauptwahlen (H):        {n_h}\n"
        f"    Neuwahlen   (N):        {n_n}\n"
        f"    Stichwahlen (S):        {n_s}\n"
        f"  Winners in Table 14:      {len(winners)}\n"
        f"  Gemeinden w/o winner:     {n_no_winner}\n"
        f"  Oberbürgermeister AGS:    {n_ob} "
        f"(expected {len(STADTKREIS_AGS)} Stadtkreise + {N_GROSSE_KREISSTAEDTE} GKS"
        f" = {len(STADTKREIS_AGS) + N_GROSSE_KREISSTAEDTE})\n"
    )
    if bad_share:
        sys.stderr.write(
            f"  WARNING: {len(bad_share)} winner share mismatches (>0.3pp):\n")
        for a, g, calc, rep in bad_share[:15]:
            sys.stderr.write(f"     {a} {g}: votes/valid={calc} vs reported={rep}\n")

    with open(OUT, "w", newline="", encoding="utf-8") as fh:
        w = csv.DictWriter(fh, fieldnames=FIELDS)
        w.writeheader()
        w.writerows(rows)
    sys.stderr.write(f"  Wrote {len(rows)} rows -> {OUT}\n")


if __name__ == "__main__":
    main()
