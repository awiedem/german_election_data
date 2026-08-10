#!/usr/bin/env python3
"""Stage-0 parser for the FULL HISTORICAL Hessen Direktwahlen series 1993-2026.

Source: `data/mayoral_elections/raw/hessen/Direktwahlen_in_Hessen_seit_1993.xlsx`
(Hessisches Statistisches Landesamt, Az. WAHL12/0005, Stand 17.07.2026; supplied
directly by HSL on request, 27.07.2026). One row per (Gemeinde/Landkreis, Wahltag,
round): full counts (Wahlberechtigte/Wähler/ungültig/gültig), the winner's
Wahlvorschlagsträger + Geschlecht + Zahl der Amtszeiten/Wiederwahlen, and up to 20
Wahlvorschlag blocks (Träger, Stimmen, %). **No candidate names** — HSL redacts
them for data-protection reasons (names of mayors whose term has ended may not be
released); the header note also warns the table only reflects what municipalities
have transmitted, so very recent elections can be missing.

This parser makes the historical file the BASE for all Hessen cycles and GRAFTS
candidate names from the existing public snapshots:
  * `he_parsed.csv` (00_he_parse_xlsx.py: May-2026 B VII m XLSX + 2024-PDF
    fallback) — winner names for the most-recent election per unit (~2017-2026).
  * `he_pdf_parsed.csv` (00_he_parse.py: May-2024 B VII m PDF) — the elected
    candidate and the named first Wahlvorschlag.  The latter recovers losing
    candidate identities that the newer XLSX fallback deliberately discarded.
  * `he2026_parsed.csv` (00_he_kommunalwahl2026_scrape.py: hessenschau, %-only)
    — ALL candidate names for the 2026 cycles, matched by within-round rank with
    a vote-share tolerance check (party vocabularies differ, so no party join).
he_parsed cycles that do not match any historical round even by result
FINGERPRINT (valid votes + exact candidate-vote multiset, the ST pattern) are
appended verbatim so coverage can never shrink.

Verified source-integrity facts (full-file scan, 3,038 data rows):
  * Wahltag/round key is unique; every count column is complete; voters-valid ==
    invalid and the printed turnout/% columns reproduce exactly (0 mismatches).
  * Multi-WV rounds: sum(WV votes) == gültige Stimmen exactly (0 mismatches).
    Single-WV rounds are Ja/Nein votes: WV votes = Ja < gültige (576 rows); the
    printed WV-% confirms the share denominator is gültige Stimmen incl. Nein.
  * Every decisive round's winner polled ≥50%; every non-decisive Hauptwahl is
    resolved by a Stichwahl or Neuwahl within a year (0 unresolved cycles).

Quirks handled (each observed in the file):
  * Footnote markers stuck to Gemeinde names ("Hanau, Brüder-Grimm-Stadt5)",
    "Allendorf (Eder)4)") — stripped.
  * Hanau is coded 435014 up to 2021 but 415000 for 2026 (kreisfrei since
    1.1.2026, footnote 5) — 415000 is overridden to the crosswalkable 06435014,
    matching 00_he_parse_xlsx.py, so all Hanau history shares one AGS.
  * `Neuwahl` rows (Driedorf 2016-07-10, Morschen 2022-09-25): a failed
    single-candidate Ja/Nein Hauptwahl months earlier triggered a completely new
    election with new candidates -> kept as its own `hauptwahl` cycle (unlike
    BW, where a Neuwahl is the standard second round weeks later).
  * Winner Träger column vs max-votes WV disagree on 9 rows — external checks
    (Raunheim 2005 = Jühe/SPD, Oberursel 2003 = Brum/SPD, Bad Nauheim 2011 =
    Häuser/CDU ...) show the max-votes WV is right and the "Träger des gewählten
    Wahlvorschlags" cell is a recording slip (usually "Einzelbewerbung") or a
    spelling variant ("AL/GRÜNE" vs "AL-Grüne") -> winner = max-votes WV,
    mismatches reported. EXCEPTION: an exact vote TIE (Ahnatal 2020-11-22, SPD
    2106 : CDU 2106, decided by lot) is broken BY the Träger column.
  * Repeated runoffs recorded as extra decisive rows: Bad Camberg's annulled
    2004-06-27 Stichwahl repeated 2005-02-20 (an orphaned Stichwahl row), and
    Bad Karlshafen's one-vote 2017-05-21 Stichwahl repeated in one Wahlbezirk on
    2017-11-05 (footnote 1; recorded as a Hauptwahl-marked row). Kept as in the
    source; the superseded round keeps its winner mark but no Amtszeiten count.
  * Date typos in the OTHER sources exposed by this file (fingerprint-matched,
    historical date wins — none of the typo dates falls on a Sunday):
    Obertshausen 2025-01-18 -> 2026-01-18 (hessenschau confirms), Neustadt
    (Hessen) 2024-01-19 -> 2025-01-19, and Herborn's decisive round mislabeled
    hauptwahl in he_parsed but correctly Stichwahl 2025-05-25 here.

Output: `he_hist_parsed.csv`, candidate-level long (same schema as he_parsed.csv
plus `winner_n_terms` / `winner_n_reelections` from the Amtszeiten columns; the
R stages ignore the extras). Landratswahl rows are split to the landrat dataset
by stage 01. Run AFTER 00_he_parse.py / 00_he_parse_xlsx.py /
00_he_kommunalwahl2026_scrape.py:
    python3 code/mayoral_elections/00_he_hist_parse.py
"""

import csv
import os
import re
import sys
import unicodedata
from collections import Counter, defaultdict

import openpyxl

HERE = os.path.dirname(os.path.abspath(__file__))
ROOT = os.path.dirname(os.path.dirname(HERE))
RAW_DIR = os.path.join(ROOT, "data", "mayoral_elections", "raw", "hessen")
XLSX = os.path.join(RAW_DIR, "Direktwahlen_in_Hessen_seit_1993.xlsx")
HE_PARSED = os.path.join(RAW_DIR, "he_parsed.csv")       # winner names (XLSX+PDF)
HE_PDF_PARSED = os.path.join(RAW_DIR, "he_pdf_parsed.csv")  # 2024 snapshot names
HE_2026 = os.path.join(RAW_DIR, "he2026_parsed.csv")     # 2026 candidate names
OUT = os.environ.get(
    "GERDA_HE_HIST_OUT", os.path.join(RAW_DIR, "he_hist_parsed.csv")
)

STATE, STATE_NAME = "06", "Hessen"
SOURCE = "Direktwahlen_in_Hessen_seit_1993.xlsx"

OB_KREISFREI = {"411", "412", "413", "414", "611"}        # Kreis codes of the 5 kfS
OB_SONDERSTATUS = {
    "06433012",  # Rüsselsheim am Main
    "06434001",  # Bad Homburg v. d. Höhe
    "06435014",  # Hanau
    "06531005",  # Gießen
    "06532023",  # Wetzlar
    "06534014",  # Marburg
    "06631009",  # Fulda
}
# Hanau appears as 435014 (<=2021) and 415000 (2026, kreisfrei) -> one AGS.
AGS_OVERRIDE = {"415000": "06435014"}

FIELDS = ["ags", "ags_name", "state", "state_name", "election_year", "election_date",
          "election_type", "round", "eligible_voters", "number_voters", "valid_votes",
          "invalid_votes", "turnout", "candidate_name", "candidate_last_name",
          "candidate_first_name", "candidate_gender", "candidate_party",
          "candidate_votes", "candidate_voteshare", "is_winner", "candidate_rank",
          "n_candidates", "winner_n_terms", "winner_n_reelections", "source_file"]

GENDER = {"männlich": "m", "weiblich": "w"}


def clean_name(x):
    """Collapse whitespace and strip trailing footnote markers like '5)'."""
    x = " ".join(str(x or "").split())
    return re.sub(r"\s*\d\)$", "", x)


def std_ags(raw6):
    if raw6 in AGS_OVERRIDE:
        return AGS_OVERRIDE[raw6]
    return "06" + raw6


def classify(ags8):
    if ags8.endswith("000"):
        return ("Oberbürgermeisterwahl" if ags8[2:5] in OB_KREISFREI
                else "Landratswahl")
    return "Oberbürgermeisterwahl" if ags8 in OB_SONDERSTATUS else "Bürgermeisterwahl"


def parse_hist():
    """Historical XLSX -> list of round dicts with WV lists."""
    wb = openpyxl.load_workbook(XLSX, read_only=True, data_only=True)
    ws = wb["Hessen_Direktwahlen_seit_1993"]
    rounds = []
    for r in ws.iter_rows(min_row=3, values_only=True):
        if not isinstance(r[0], int):          # footnote / separator rows
            continue
        raw6 = str(r[1]).strip()
        ags = std_ags(raw6)
        marker = str(r[3] or "").strip()
        # Neuwahl = a NEW election months after a failed Ja/Nein Hauptwahl,
        # with new candidates -> its own hauptwahl cycle (see docstring).
        rnd = "stichwahl" if marker == "Stichwahl" else "hauptwahl"
        date = r[4].date().isoformat()
        valid = int(r[10])
        wvs = []
        j = 15
        while j + 2 < len(r):
            traeger = str(r[j] or "").strip()
            votes = r[j + 1]
            if votes is not None:
                wvs.append({"party": traeger, "votes": int(votes),
                            "pct": float(r[j + 2])})
            j += 3
        assert wvs, f"no Wahlvorschläge at {ags} {date}"
        # winner: only on decisive rows (Träger column filled); the winner is the
        # max-votes WV; an exact tie is broken by the Träger column (Ahnatal 2020).
        win_traeger = str(r[11] or "").strip()
        winner = None
        if win_traeger:
            mx = max(w["votes"] for w in wvs)
            top = [w for w in wvs if w["votes"] == mx]
            if len(top) > 1:
                by_col = [w for w in top if w["party"] == win_traeger]
                assert by_col, f"unbreakable vote tie at {ags} {date}"
                winner = by_col[0]
            else:
                winner = top[0]
            assert winner["votes"] / valid >= 0.5 - 1e-9, \
                f"decisive winner below 50% at {ags} {date}"
        rounds.append({
            "ags": ags, "name": clean_name(r[2]), "etype": classify(ags),
            "round": rnd, "neuwahl": marker == "Neuwahl", "date": date,
            "eligible": int(r[5]), "voters": int(r[7]), "invalid": int(r[8]),
            "valid": valid, "turnout": round(float(r[6]) / 100, 6),
            "gender": GENDER.get(str(r[12] or "").strip(), ""),
            "win_traeger": win_traeger, "winner": winner,
            "n_terms": r[13] if isinstance(r[13], int) else "",
            "n_reelect": r[14] if isinstance(r[14], int) else "",
            "wvs": wvs,
        })
    return rounds


def report_winner_traeger_mismatches(rounds):
    n = 0
    for rd in rounds:
        if rd["winner"] and rd["winner"]["party"] != rd["win_traeger"]:
            n += 1
            print(f"  winner-Träger cell disagrees (max-votes WV wins): "
                  f"{rd['ags']} {rd['name'][:30]} {rd['date']} "
                  f"cell='{rd['win_traeger']}' wv='{rd['winner']['party']}'")
    # 9 known cases; a jump means the layout or winner logic drifted.
    assert n <= 12, f"{n} winner-Träger mismatches (expected ~9) — check parsing"
    return n


def _norm_person_name(x):
    """Order-insensitive comparison key for snapshot-vs-history name checks."""
    text = str(x or "").casefold().replace("ß", "ss")
    text = "".join(c for c in unicodedata.normalize("NFKD", text)
                   if not unicodedata.combining(c))
    tokens = re.findall(r"[0-9a-z]+", text)
    return "".join(sorted(t for t in tokens if t not in {"dr", "prof"}))


def _snapshot_target(key, rows, by_key, by_ags):
    """Resolve a public-snapshot cycle to one historical round.

    Exact keys cover all ordinary cases.  The same-date fallback handles the
    Morschen 2022 Neuwahl, which the PDF labels Stichwahl while the historical
    file correctly records it as a new Hauptwahl.  The strict full-result
    fingerprint retains the older protection against date typos.
    """
    target = by_key.get(key)
    if target is not None:
        return target, "exact"

    same_date = [rd for rd in by_ags.get(key[0], []) if rd["date"] == key[1]]
    if len(same_date) == 1:
        return same_date[0], "same-date"

    votes = sorted(int(float(r["candidate_votes"])) for r in rows
                   if r.get("candidate_votes", "") not in ("", "NA"))
    valid = rows[0].get("valid_votes", "")
    candidates = [
        rd for rd in by_ags.get(key[0], [])
        if votes and valid and rd["valid"] == int(float(valid))
        and sorted(w["votes"] for w in rd["wvs"]) == votes
    ]
    if len(candidates) == 1:
        return candidates[0], "fingerprint"
    return None, "unmatched"


def graft_snapshot(rounds, path, label, append_absent=False):
    """Attach every identifiable candidate name in one public snapshot.

    Winners map to the historical winner even when the snapshot suppresses the
    winner's vote count.  Other named candidates map by exact votes, with party
    used only to break vote ties.  Existing names are validated and preserved;
    this routine never overwrites one source with another.
    """
    if not os.path.exists(path):
        print(f"  {os.path.basename(path)} not found — no names grafted")
        return [], {"grafted": 0, "already": 0, "unmatched": 0,
                    "redated": 0, "conflicts": 0}
    by_key = {}
    by_ags = defaultdict(list)
    for rd in rounds:
        by_key[(rd["ags"], rd["date"], rd["round"])] = rd
        by_ags[rd["ags"]].append(rd)

    groups = defaultdict(list)
    for row in csv.DictReader(open(path, encoding="utf-8")):
        groups[(row["ags"], row["election_date"], row["round"])].append(row)

    stats = {"grafted": 0, "already": 0, "unmatched": 0,
             "redated": 0, "conflicts": 0}
    appended = []
    for key, rows in sorted(groups.items()):
        target, match_method = _snapshot_target(key, rows, by_key, by_ags)
        if target is None:
            named_rows = [r for r in rows if r.get("candidate_name", "").strip()]
            stats["unmatched"] += len(named_rows)
            if append_absent:
                appended.extend(rows)
                print(f"  {label} cycle ABSENT from historical file, appended: "
                      f"{key[0]} {rows[0]['ags_name'][:25]} {key[1]} {key[2]}")
            continue

        if match_method != "exact":
            stats["redated"] += 1
            print(f"  {label} {match_method} match -> historical key: "
                  f"{key[0]} {rows[0]['ags_name'][:25]} {key[1]}/{key[2]} "
                  f"-> {target['date']}/{target['round']}")

        for row in rows:
            name = row.get("candidate_name", "").strip()
            if not name:
                continue

            candidate = None
            if row.get("is_winner") == "TRUE" and target["winner"] is not None:
                candidate = target["winner"]
            else:
                raw_vote = row.get("candidate_votes", "")
                if raw_vote not in ("", "NA"):
                    vote = int(float(raw_vote))
                    matches = [w for w in target["wvs"] if w["votes"] == vote]
                    party = row.get("candidate_party", "").strip()
                    if len(matches) > 1 and party:
                        party_matches = [w for w in matches if w["party"] == party]
                        if len(party_matches) == 1:
                            matches = party_matches
                    if len(matches) == 1:
                        candidate = matches[0]

            if candidate is None:
                stats["unmatched"] += 1
                print(f"  WARNING: named {label} candidate could not be matched: "
                      f"{key} '{name}' votes={row.get('candidate_votes', '')}")
                continue

            if candidate.get("name", ""):
                if _norm_person_name(candidate["name"]) != _norm_person_name(name):
                    stats["conflicts"] += 1
                    print(f"  WARNING: {label} name '{name}' conflicts with "
                          f"already-grafted '{candidate['name']}' at {key}")
                else:
                    stats["already"] += 1
                continue

            candidate["name"] = name
            candidate["last"] = row.get("candidate_last_name", "").strip()
            candidate["first"] = row.get("candidate_first_name", "").strip()
            stats["grafted"] += 1

            if candidate is target["winner"]:
                party = row.get("candidate_party", "").strip()
                if party and party != candidate["party"]:
                    print(f"  note: grafted winner party label differs: {key} "
                          f"{label}='{party}' hist='{candidate['party']}'")
                gender = row.get("candidate_gender", "").strip()
                gender = {"male": "m", "female": "w"}.get(gender, gender)
                if gender and target["gender"] and gender != target["gender"]:
                    print(f"  WARNING: winner gender disagrees at {key}: "
                          f"{label}={gender} hist={target['gender']}")

    return appended, stats


def graft_hessenschau(rounds):
    """Attach ALL candidate names for 2026 cycles by within-round rank match."""
    if not os.path.exists(HE_2026):
        print("  he2026_parsed.csv not found — no 2026 names grafted")
        return 0, 0
    by_key = {(rd["ags"], rd["date"], rd["round"]): rd for rd in rounds}
    groups = defaultdict(list)
    for row in csv.DictReader(open(HE_2026, encoding="utf-8")):
        groups[(row["ags"], row["election_date"], row["round"])].append(row)

    grafted, skipped = 0, 0
    for key, rows in sorted(groups.items()):
        target = by_key.get(key)
        if target is None:
            continue  # not (yet) transmitted to HSL -> stays in the he2026 flow
        hs = sorted(rows, key=lambda r: -float(r["candidate_voteshare"]))
        wv = sorted(target["wvs"], key=lambda w: -w["votes"])
        if len(hs) != len(wv):
            print(f"  WARNING: candidate count differs at {key}: "
                  f"hessenschau {len(hs)} vs historical {len(wv)} — names skipped")
            skipped += len(hs)
            continue
        for h, w in zip(hs, wv):
            share = w["votes"] / target["valid"]
            if abs(share - float(h["candidate_voteshare"])) > 0.015:
                print(f"  WARNING: share mismatch at {key} rank "
                      f"{h['candidate_name']}: hessenschau "
                      f"{h['candidate_voteshare']} vs {share:.3f} — name skipped")
                skipped += 1
                continue
            if "name" not in w:          # he_parsed (official) grafts win
                w["name"] = h["candidate_name"].strip()
                w["last"] = h["candidate_last_name"].strip()
                w["first"] = h["candidate_first_name"].strip()
                grafted += 1
            elif h["candidate_last_name"].strip() and \
                    h["candidate_last_name"].strip().lower() not in w["last"].lower():
                print(f"  WARNING: hessenschau name '{h['candidate_name']}' vs "
                      f"already-grafted '{w['name']}' at {key}")
        winner_hs = next((r for r in rows if r["is_winner"] == "TRUE"), None)
        if winner_hs is not None and target["winner"] is not None:
            top = max(target["wvs"], key=lambda w: w["votes"])
            if abs(float(winner_hs["candidate_voteshare"])
                   - top["votes"] / target["valid"]) > 0.015:
                print(f"  WARNING: winner disagreement at {key}")
    return grafted, skipped


def propagate_stichwahl_names(rounds):
    """Propagate candidate names between Hauptwahl and Stichwahl records.

    Stage 01b pairs the two rounds by public identity.  A name published only
    for the runoff must therefore also be present on the corresponding first-
    round record; otherwise one real candidate becomes two wide rows.  Copy
    only when the candidate's Träger is unique in BOTH rounds (redacted
    ``Einzelbewerbung`` twins remain deliberately unmatched).
    """
    by_ags = defaultdict(list)
    for rd in rounds:
        by_ags[rd["ags"]].append(rd)
    n = 0
    for rd in rounds:
        if rd["round"] != "stichwahl":
            continue
        hw = [x for x in by_ags[rd["ags"]]
              if x["round"] == "hauptwahl" and x["date"] < rd["date"]
              and (int(rd["date"][:4]) - int(x["date"][:4])) <= 1]
        hw = max(hw, key=lambda x: x["date"], default=None)
        if hw is None or (_days_between(hw["date"], rd["date"]) >= 60):
            continue
        for w in rd["wvs"]:
            if sum(1 for v in rd["wvs"] if v["party"] == w["party"]) != 1:
                continue
            cands = [v for v in hw["wvs"] if v["party"] == w["party"]]
            if len(cands) != 1:
                continue
            h = cands[0]
            if "name" in w and "name" not in h:
                h.update(name=w["name"], last=w["last"], first=w["first"])
                n += 1
            elif "name" in h and "name" not in w:
                w.update(name=h["name"], last=h["last"], first=h["first"])
                n += 1
            elif "name" in h and "name" in w and \
                    _norm_person_name(h["name"]) != _norm_person_name(w["name"]):
                print(f"  WARNING: round-name conflict at {rd['ags']} "
                      f"{hw['date']}/{rd['date']} party={w['party']}: "
                      f"HW='{h['name']}' SW='{w['name']}'")
    return n


def _days_between(d1, d2):
    from datetime import date
    a = date.fromisoformat(d1)
    b = date.fromisoformat(d2)
    return (b - a).days


def emit(rounds, appended):
    out = []
    for rd in rounds:
        ranked = sorted(rd["wvs"], key=lambda w: -w["votes"])
        rank_of = {id(w): i + 1 for i, w in enumerate(ranked)}
        for w in rd["wvs"]:
            is_w = rd["winner"] is not None and w is rd["winner"]
            share = round(w["votes"] / rd["valid"], 6) if rd["valid"] else ""
            out.append({
                "ags": rd["ags"], "ags_name": rd["name"],
                "state": STATE, "state_name": STATE_NAME,
                "election_year": rd["date"][:4], "election_date": rd["date"],
                "election_type": rd["etype"], "round": rd["round"],
                "eligible_voters": rd["eligible"], "number_voters": rd["voters"],
                "valid_votes": rd["valid"], "invalid_votes": rd["invalid"],
                "turnout": rd["turnout"],
                "candidate_name": w.get("name", ""),
                "candidate_last_name": w.get("last", ""),
                "candidate_first_name": w.get("first", ""),
                "candidate_gender": rd["gender"] if is_w else "",
                "candidate_party": w["party"],
                "candidate_votes": w["votes"], "candidate_voteshare": share,
                "is_winner": "TRUE" if is_w else "FALSE",
                "candidate_rank": rank_of[id(w)], "n_candidates": len(rd["wvs"]),
                "winner_n_terms": rd["n_terms"] if is_w else "",
                "winner_n_reelections": rd["n_reelect"] if is_w else "",
                "source_file": SOURCE,
            })
    for r in appended:                      # he_parsed rows the hist file lacks
        out.append({**{k: "" for k in FIELDS},
                    **{k: r.get(k, "") for k in FIELDS if k in r}})
    out.sort(key=lambda x: (x["ags"], str(x["election_date"]), x["round"],
                            int(x["candidate_rank"] or 99)))
    return out


def main():
    if not os.path.exists(XLSX):
        raise SystemExit(f"historical XLSX not found: {XLSX}")

    rounds = parse_hist()
    n_rounds = len(rounds)
    n_dec = sum(1 for rd in rounds if rd["winner"] is not None)
    keys = Counter((rd["ags"], rd["date"], rd["round"]) for rd in rounds)
    assert max(keys.values()) == 1, f"duplicate rounds: {[k for k,v in keys.items() if v>1]}"

    print("=== Hessen Direktwahlen 1993-2026 (HSL historical file) ===")
    print(f"  rounds: {n_rounds} ({n_dec} decisive) | units: "
          f"{len(set(rd['ags'] for rd in rounds))} | years: "
          f"{min(rd['date'][:4] for rd in rounds)}-{max(rd['date'][:4] for rd in rounds)}")
    n_mm = report_winner_traeger_mismatches(rounds)
    print(f"  winner-Träger cell disagreements (max-votes WV used): {n_mm}")

    # Share sanity vs the printed WV-% column. A handful of rows carry a
    # 1-decimal-rounded % and Schlüchtern 2010-05-30 printed % computed on a
    # denominator of 6,246 while the file's gültige Stimmen is 6,216 (source-
    # internal slip; votes/valid is what we publish) — so tolerate those.
    off = [(abs(w["votes"] / rd["valid"] * 100 - w["pct"]), rd, w)
           for rd in rounds for w in rd["wvs"]]
    gross = [x for x in off if x[0] > 0.06]
    for d, rd, w in gross:
        print(f"  note: printed % off by {d:.3f} pp at {rd['ags']} "
              f"{rd['name'][:25]} {rd['date']} {w['party']} (known source slip)")
    assert len(gross) <= 2, f"{len(gross)} rows with printed-% deviation > 0.06 pp"
    print(f"  max |votes/valid - printed %|: {max(x[0] for x in off):.4f} pp")

    print("\n--- grafting names from he_parsed.csv ---")
    appended, latest_stats = graft_snapshot(
        rounds, HE_PARSED, "he_parsed", append_absent=True
    )
    print(f"  names grafted: {latest_stats['grafted']} | already present: "
          f"{latest_stats['already']} | unmatched: {latest_stats['unmatched']} "
          f"| non-exact cycle matches: {latest_stats['redated']} | conflicts: "
          f"{latest_stats['conflicts']} | appended rows: {len(appended)}")

    print("\n--- grafting additional names from May-2024 PDF snapshot ---")
    _, pdf_stats = graft_snapshot(
        rounds, HE_PDF_PARSED, "he_pdf_parsed", append_absent=False
    )
    print(f"  names grafted: {pdf_stats['grafted']} | already present: "
          f"{pdf_stats['already']} | unmatched: {pdf_stats['unmatched']} "
          f"| non-exact cycle matches: {pdf_stats['redated']} | conflicts: "
          f"{pdf_stats['conflicts']}")
    assert pdf_stats["grafted"] + pdf_stats["already"] == 671, (
        "May-2024 PDF should contribute or confirm all 671 published names; "
        f"got {pdf_stats}"
    )
    assert pdf_stats["unmatched"] == 0 and pdf_stats["conflicts"] == 0

    print("\n--- grafting 2026 candidate names from hessenschau ---")
    hs_grafted, hs_skipped = graft_hessenschau(rounds)
    print(f"  candidate names grafted: {hs_grafted} | skipped: {hs_skipped}")

    n_prop = propagate_stichwahl_names(rounds)
    print(f"  Stichwahl candidate names propagated to their Hauptwahl row: {n_prop}")

    out = emit(rounds, appended)
    by_type = Counter(x["election_type"] for x in out)
    named = sum(1 for x in out if x["candidate_name"])
    print(f"\n  candidate rows: {len(out)} ({named} with a name) | by type: "
          f"{dict(by_type)}")

    with open(OUT, "w", newline="", encoding="utf-8") as fh:
        w = csv.DictWriter(fh, fieldnames=FIELDS)
        w.writeheader()
        w.writerows(out)
    print(f"  wrote {len(out)} rows -> {OUT}")


if __name__ == "__main__":
    main()
