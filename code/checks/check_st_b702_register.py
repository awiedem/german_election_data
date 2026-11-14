#!/usr/bin/env python3
"""Compare the StaLA Sachsen-Anhalt registers B VII 702 with GERDA.

Reads the outputs of code/mayoral_elections/00_st_b702_parse.py and checks them
against GERDA's own Sachsen-Anhalt sources:

  1. every Bürgermeister in a register (2.2 lists, the post-Stichtag and the
     dissolved-Gemeinde lists) is matched to a GERDA election
     (raw/sachsen_anhalt/st_stala_parsed.csv) by (AGS, Wahltag), then
     (Wahltag, Gemeinde name within the same Kreis lineage), then (Wahltag,
     winner surname); the winner's name, party, birth year and runoff are
     compared;
  2. register elections GERDA lacks on that date are classified (missing
     election, same winner on another date, another election nearby);
  3. the OB and Landrat results in the annexes of the 2007 and 2009 issues are
     compared with mayoral and Landrat GERDA data (the Landrat restricted twin,
     which keeps the losers' names).

Writes (gitignored, they carry names):
  stala_b702/st_b702_compare_restricted.csv
  stala_b702/st_b702_gaps_restricted.csv
  stala_b702/st_b702_annex_compare_restricted.csv

Run: python3 code/checks/check_st_b702_register.py
"""
from __future__ import annotations

import csv
import difflib
import re
import unicodedata
from collections import Counter, defaultdict
from datetime import date
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
ST_RAW = ROOT / "data" / "mayoral_elections" / "raw" / "sachsen_anhalt"
B702 = ST_RAW / "stala_b702"
REGISTER = B702 / "st_b702_register_restricted.csv"
RESULTS = B702 / "st_b702_results_restricted.csv"
ST_PARSED = ST_RAW / "st_stala_parsed.csv"
# The source file as parsed BEFORE the register pins (written by
# 00_st_hist_parse.py). Pins are derived from it; deriving them from the patched
# output would make them disappear (code review 2026-11-13). --after compares
# the patched output instead, to see what is left.
ST_BEFORE = B702 / "st_stala_parsed_before_b702_restricted.csv"
import sys as _sys
GERDA_INPUT = ST_PARSED if "--after" in _sys.argv else ST_BEFORE
LANDRAT = ROOT / "data" / "landrat_elections" / "final_restricted" / "landrat_candidates_restricted.csv"

# Kreise before the Kreisgebietsreform of 1 July 2007 -> Kreise after it,
# including the Gemeinden that changed Kreis (Loburg area of Anhalt-Zerbst to
# Jerichower Land, Falkenstein/Harz to Harz, Kläden/Jerchel area to Stendal)
OLD_TO_NEW_KREIS = {
    "15101": {"15001"}, "15202": {"15002"}, "15303": {"15003"},
    "15151": {"15082", "15091", "15001", "15086"}, "15153": {"15089"}, "15154": {"15082"},
    "15159": {"15082"}, "15171": {"15091"}, "15256": {"15084"}, "15260": {"15087"},
    "15261": {"15088"}, "15265": {"15088"}, "15266": {"15087"}, "15268": {"15084"},
    "15352": {"15089", "15085"}, "15355": {"15083"}, "15357": {"15085"}, "15358": {"15086"},
    "15362": {"15083"}, "15363": {"15090"}, "15364": {"15085"}, "15367": {"15089"},
    "15369": {"15085"}, "15370": {"15081", "15090"},
}


def same_lineage(k1: str, k2: str) -> bool:
    return k1 == k2 or k1 in OLD_TO_NEW_KREIS.get(k2, ()) or k2 in OLD_TO_NEW_KREIS.get(k1, ())


def norm(s: str) -> str:
    s = s or ""
    if not s.isupper():
        s = s.replace("I", "l")  # OCR reads l as I
    s = unicodedata.normalize("NFKD", s).encode("ascii", "ignore").decode().lower().replace("ss", "s")
    return re.sub(r"[^a-z]", "", s)


def norm_unit(s: str) -> str:
    s = re.sub(r"^[^A-Za-zÄÖÜ]+", "", s or "")
    s = re.sub(r",\s*(Stadt|Hansestadt|Lutherstadt|Landeshauptstadt|Flecken)$", "", s)
    s = re.sub(r"\(.*?\)", "", s)
    return norm(s)


PARTY = {"pds": "linke", "dielinke": "linke", "linke": "linke", "cdu": "cdu", "spd": "spd",
         "fdp": "fdp", "wg": "wg", "grune": "grune", "bundnis90diegrunen": "grune"}


def norm_party(s: str) -> str:
    k = norm(s)
    return PARTY.get(k, k)


def sunday(d: str) -> bool | str:
    try:
        return date.fromisoformat(d).weekday() == 6
    except ValueError:
        return ""


def write(path: Path, rows: list[dict]) -> None:
    cols: list[str] = []
    for r in rows:
        cols += [k for k in r if k not in cols]
    with path.open("w", newline="", encoding="utf-8") as fh:
        w = csv.DictWriter(fh, fieldnames=cols)
        w.writeheader()
        w.writerows(rows)


def load_gerda():
    if not GERDA_INPUT.exists():
        raise SystemExit(f"missing {GERDA_INPUT}: run 00_st_hist_parse.py first")
    st = list(csv.DictReader(GERDA_INPUT.open(encoding="utf-8")))
    el = {}
    for r in st:
        if r["election_type"] not in ("Bürgermeisterwahl", "Oberbürgermeisterwahl"):
            continue
        e = el.setdefault((r["ags"], r["election_date"], r["round"]), {
            "ags": r["ags"], "name": r["ags_name"], "date": r["election_date"], "round": r["round"],
            "type": r["election_type"], "winner": None, "n": 0, "birth_year": r["winner_birth_year"]})
        e["n"] += 1
        if r["is_winner"] == "True":
            e["winner"] = r
    return st, el


def compare_register(el: dict) -> tuple[list[dict], list[dict]]:
    by_ags_date, by_date_unit, by_date_surname = {}, defaultdict(list), defaultdict(list)
    by_ags, by_name = defaultdict(list), defaultdict(list)
    runoffs, runoff_by_ags = {}, defaultdict(list)
    for (ags, d, rnd), e in el.items():
        by_ags[ags].append(e)
        by_name[norm_unit(e["name"])].append(e)
        if rnd == "hauptwahl":
            by_ags_date[(ags, d)] = e
            by_date_unit[(d, norm_unit(e["name"]))].append(e)
        else:
            runoffs[(ags, d)] = e
            runoff_by_ags[ags].append(d)
        if e["winner"] is not None and rnd == "hauptwahl":
            by_date_surname[(d, norm(e["winner"]["candidate_last_name"]))].append(e)

    reg = [r for r in csv.DictReader(REGISTER.open(encoding="utf-8"))
           if r["office"] == "Bürgermeister" and r["section"] != "bm_ha"]
    out, gaps = [], []
    for r in reg:
        hw, kr = r["election_date"], r["ags"][:5]
        res = {"stand": r["stand"], "section": r["section"], "ags_reg": r["ags"], "unit_reg": r["unit_name"],
               "flag_reg": r["office_flag"], "acting": r["acting"], "hw_reg": hw, "sw_reg": r["runoff_date"],
               "last_reg": r["last_name"], "first_reg": r["first_name"], "party_reg": r["party"],
               "by_reg": r["birth_year"], "ncand_reg": r["n_candidates"], "profession_reg": r["profession"],
               "page": r["page"], "footnotes": r["page_footnotes"]}
        e, how = None, ""
        if r["acting"] == "True" or not hw:
            how = "no_election_in_register"
        else:
            e = by_ags_date.get((r["ags"], hw))
            how = "ags_date" if e else ""
            def same_winner(x):
                return x["winner"] is not None and norm(x["winner"]["candidate_last_name"]) == norm(r["last_name"])

            if not e:
                # same date and Gemeinde name: within the Kreis lineage, or anywhere
                # when the winner agrees too (GERDA files some elections under a
                # code of another Kreis, see gerda_code_other_kreis)
                c = [x for x in by_date_unit.get((hw, norm_unit(r["unit_name"])), [])
                     if same_lineage(kr, x["ags"][:5]) or same_winner(x)]
                if len(c) > 1:
                    c = [x for x in c if same_winner(x)] or c
                if len(c) == 1:
                    e, how = c[0], "date_name"
            if not e:
                c = [x for x in by_date_surname.get((hw, norm(r["last_name"])), [])
                     if same_lineage(kr, x["ags"][:5])
                     or difflib.SequenceMatcher(None, norm_unit(x["name"]), norm_unit(r["unit_name"])).ratio() > 0.8]
                if len(c) == 1:
                    e, how = c[0], "date_surname"
        res["match"] = how or "unmatched"
        if e:
            res["gerda_code_other_kreis"] = not same_lineage(kr, e["ags"][:5])
        if e:
            # the cycle: a runoff of the same code within 60 days (as in 00_st_hist_parse.py)
            later_sw = sorted(d for d in runoff_by_ags[e["ags"]]
                              if 0 < (date.fromisoformat(d) - date.fromisoformat(e["date"])).days <= 60)
            dec = e["winner"]
            if later_sw and runoffs[(e["ags"], later_sw[0])]["winner"] is not None:
                dec = runoffs[(e["ags"], later_sw[0])]["winner"]
            res.update({
                "ags_gerda": e["ags"], "unit_gerda": e["name"], "hw_gerda": e["date"],
                "sw_gerda": ",".join(later_sw), "type_gerda": e["type"],
                "last_gerda": dec["candidate_last_name"] if dec else "",
                "first_gerda": dec["candidate_first_name"] if dec else "",
                "party_gerda": dec["candidate_party"] if dec else "",
                "by_gerda": e["birth_year"], "n_cand_gerda": e["n"],
            })
            res["same_last"] = norm(res["last_gerda"]) == norm(r["last_name"])
            res["last_similarity"] = round(difflib.SequenceMatcher(
                None, norm(res["last_gerda"]), norm(r["last_name"])).ratio(), 2)
            res["same_first"] = norm(res["first_gerda"]).startswith(norm(r["first_name"])[:4]) if r["first_name"] else ""
            res["same_party"] = norm_party(res["party_gerda"]) == norm_party(r["party"])
            res["same_by"] = (res["by_gerda"] == r["birth_year"]) if (res["by_gerda"] and r["birth_year"]) else ""
            res["runoff_reg_in_gerda"] = (r["runoff_date"] in res["sw_gerda"]) if r["runoff_date"] else ""
            res["runoff_gerda_not_reg"] = bool(res["sw_gerda"]) and not r["runoff_date"]
        elif res["match"] == "unmatched":
            seen = {}
            for x in by_ags.get(r["ags"], []) + by_name.get(norm_unit(r["unit_name"]), []):
                if x["ags"] == r["ags"] or same_lineage(kr, x["ags"][:5]):
                    seen[(x["ags"], x["date"], x["round"])] = x
            cands = sorted(seen.values(), key=lambda x: x["date"])

            def wname(x):
                return x["winner"]["candidate_last_name"] if x["winner"] is not None else ""

            near = [x for x in cands if abs((date.fromisoformat(x["date"]) - date.fromisoformat(hw)).days) < 400]
            same = [x for x in near if norm(wname(x)) == norm(r["last_name"])]
            cls = ("gemeinde_not_in_gerda" if not cands else "same_winner_other_date" if same
                   else "other_election_nearby" if near else "election_missing_in_gerda")
            prev = [x for x in cands if x["date"] < hw]
            nxt = [x for x in cands if x["date"] > hw]
            fmt = lambda x: f'{x["date"]} {x["round"][:2]} {wname(x)} [{x["ags"]}]'  # noqa: E731
            gaps.append({**res, "class": cls, "hw_is_sunday": sunday(hw),
                         "gerda_prev": fmt(prev[-1]) if prev else "", "gerda_next": fmt(nxt[0]) if nxt else "",
                         "gerda_same_winner_dates": "; ".join(x["date"] for x in same)})
        out.append(res)
    return out, gaps


def compare_annex(st: list[dict]) -> list[dict]:
    ann = list(csv.DictReader(RESULTS.open(encoding="utf-8")))
    lr = list(csv.DictReader(LANDRAT.open(encoding="utf-8"))) if LANDRAT.exists() else []
    old_ob = {"15001": "15101", "15002": "15202", "15003": "15303"}
    out = []
    seen = set()
    for r in sorted(ann, key=lambda x: x["stand"]):
        key = (r["kreis"], r["election_type"], r["hw_date"], norm(r["last_name"]))
        if key in seen:
            continue  # the 2009 issue repeats the 2007 annex; both agree exactly
        seen.add(key)
        g_hw = g_sw = ""
        g_el = {}
        if r["election_type"] == "Landratswahl":
            g = [x for x in lr if x["ags"][:5] == r["kreis"] and x["election_date"] == r["hw_date"]]
            hit = [x for x in g if norm(x["candidate_last_name"]) == norm(r["last_name"])]
            if hit:
                g_hw, g_sw = hit[0]["candidate_votes_hw"], hit[0]["candidate_votes_sw"]
            if g:
                g_el = {"g_eligible": g[0]["eligible_voters"], "g_voters": g[0]["number_voters"],
                        "g_valid": g[0]["valid_votes"], "g_flag_decisive_round_missing": g[0]["flag_decisive_round_missing"],
                        "g_is_winner": hit[0]["is_winner"] if hit else ""}
        else:
            codes = {r["kreis"] + "000", old_ob.get(r["kreis"], "x") + "000"}
            g = [x for x in st if x["ags"] in codes and x["election_date"] in (r["hw_date"], r["sw_date"])
                 and x["election_type"] == "Oberbürgermeisterwahl"]
            for x in g:
                if norm(x["candidate_last_name"]) == norm(r["last_name"]):
                    if x["round"] == "hauptwahl":
                        g_hw = x["candidate_votes"]
                    else:
                        g_sw = x["candidate_votes"]
            gh = [x for x in g if x["round"] == "hauptwahl"]
            if gh:
                g_el = {"g_eligible": gh[0]["eligible_voters"], "g_voters": gh[0]["number_voters"], "g_valid": gh[0]["valid_votes"]}
        num = lambda v: str(v).split(".")[0] if v not in ("", None) else ""  # noqa: E731
        out.append({**{k: r[k] for k in ("stand", "kreis", "unit", "election_type", "hw_date", "sw_date",
                                         "eligible_hw", "voters_hw", "valid_hw", "voters_sw", "valid_sw",
                                         "last_name", "first_name", "party", "votes_hw", "votes_sw")},
                    "gerda_votes_hw": num(g_hw), "gerda_votes_sw": num(g_sw), **g_el,
                    "in_gerda": bool(g_el),
                    "hw_agrees": num(g_hw) == r["votes_hw"],
                    "sw_agrees": (num(g_sw) == r["votes_sw"]) if r["votes_sw"] else ""})
    return out


# ---------------------------------------------------------------- pins

PARTY_OK = {"CDU": "CDU", "SPD": "SPD", "FDP": "FDP", "WG": "WG", "PDS": "PDS",
            "DIE LINKE": "DIE LINKE", "Die Linke.": "Die Linke.", "Die Linke": "Die Linke.",
            "GRÜNE": "GRÜNE", "NPD": "NPD"}
META = ROOT / "data" / "mayoral_elections" / "metadata"
CROSSWALK = ROOT / "data" / "crosswalks" / "final" / "ags_crosswalks.csv"


# Register elections the gap classifier puts next to another GERDA election,
# checked by hand against the scans and GERDA (2026-11-13): genuinely missing.
MANUAL_ADD = {
    ("2004-01-01", "15370026", "2002-05-26"),  # Engersen: by-election after the 2001 mayor; in the 2004, 2007 and 2009 issues
    ("2004-01-01", "15154028", "2002-04-21"),  # Retzau: footnote: resigned 01.03.2004, Neuwahl 13.06.2004 (in GERDA)
    ("2010-03-01", "15086005", "2009-08-16"),  # Biederitz: the old Gemeinde (list 2.4), before the Einheitsgemeinde's November election
}


def guard_pin_loss(path: Path, new_rows: list[dict], key_cols: tuple) -> None:
    """Refuse to overwrite a pin table with one that drops existing pins."""
    if not path.exists() or "--force" in _sys.argv:
        return
    old = {tuple(r[k] for k in key_cols) for r in csv.DictReader(path.open(encoding="utf-8"))}
    new = {tuple(str(r[k]) for k in key_cols) for r in new_rows}
    lost = old - new
    if lost:
        raise SystemExit(f"{path.name}: the new derivation drops {len(lost)} pin(s), e.g. "
                         f"{sorted(lost)[:3]}; check the input (unpatched?) or pass --force")


def write_pins(st: list[dict], cmp_rows: list[dict], gaps: list[dict]) -> None:
    """Derive the tracked pin tables that 00_st_hist_parse.py applies.

    st_b702_added_elections.csv: register elections GERDA lacks. Excluded:
      anything GERDA holds on the same date (or runoff date) with the same winner
      surname anywhere in the state (renamed or misspelt Gemeinden), acting
      mayors and records without a date.
    st_b702_fills.csv: the winner's party (whitelisted labels, all issues
      agree) where GERDA's matched election has none.
    stala_b702/st_b702_birth_year_pins_restricted.csv (gitignored): the
      register birth year where GERDA's matched election has none. Register
      birth years are not published (decision of 2026-11-13), so they never go
      into a tracked file; hand-made rows (origin "correction") are kept.
    Only codes, dates and labels are written to tracked files: names are read
    from the restricted register at build time.
    """
    reg = list(csv.DictReader(REGISTER.open(encoding="utf-8")))
    by_date_surname = defaultdict(list)
    for r in st:
        by_date_surname[(r["election_date"], norm(r["candidate_last_name"]))].append(r)
    cw = defaultdict(set)
    for r in csv.DictReader(CROSSWALK.open(encoding="utf-8")):
        if r["ags"].zfill(8).startswith("15"):
            cw[r["ags"].zfill(8)].add(int(r["year"]))

    def old_code(g: dict) -> str:
        """AGS am Wahltag for a pre-reform election listed under a post-2007 code."""
        kr, nu = g["ags_reg"][:5], norm_unit(g["unit_reg"])
        found = {r["ags"] for r in reg if r["stand"] == "2004-01-01" and r["section"] == "bm"
                 and norm_unit(r["unit_name"]) == nu and same_lineage(kr, r["ags"][:5])}
        found |= {r["ags"] for r in st if norm_unit(r["ags_name"]) == nu and r["election_date"] < "2007-07-01"
                  and same_lineage(kr, r["ags"][:5]) and not is_new(r["ags"][:5])}
        if len(found) != 1:
            raise SystemExit(f"no unique pre-2007 code for {g['unit_reg']} {g['ags_reg']}: {found}")
        return found.pop()

    added, seen = [], {}
    for g in gaps:
        if g["class"] != "election_missing_in_gerda" and \
                (g["stand"], g["ags_reg"], g["hw_reg"]) not in MANUAL_ADD:
            continue
        k = (norm_unit(g["unit_reg"]), g["hw_reg"])
        seen.setdefault(k, []).append(g)
    for k, gs in sorted(seen.items(), key=lambda kv: kv[0][1]):
        g = sorted(gs, key=lambda x: x["stand"])[0]
        dates = [g["hw_reg"]] + ([g["sw_reg"]] if g["sw_reg"] else [])
        # GERDA may hold the election under another spelling or a later name of
        # the Gemeinde (Siersleben/"Siersieben", Zerbst vs Zerbst/Anhalt): same
        # date, same winner surname, same Kreis lineage and a similar name. A
        # common surname in another Gemeinde of the Kreis does not count.
        hits = [h for d in dates for h in by_date_surname.get((d, norm(g["last_reg"])), [])
                if h["is_winner"] == "True" and same_lineage(g["ags_reg"][:5], h["ags"][:5])
                and difflib.SequenceMatcher(None, norm_unit(h["ags_name"]), norm_unit(g["unit_reg"])).ratio() > 0.6]
        if hits:
            print(f"  pin: skipped {g['unit_reg']} {g['hw_reg']}: GERDA holds it as "
                  f"{hits[0]['ags_name']} [{hits[0]['ags']}]")
            continue
        names = {(norm(x["last_reg"]), norm(x["first_reg"])) for x in gs}
        if len(names) > 1:
            raise SystemExit(f"issues disagree on the winner of {g['unit_reg']} {g['hw_reg']}: {names}")
        pre = g["hw_reg"] < "2007-07-01" and is_new(g["ags_reg"][:5])
        ags = old_code(g) if pre else g["ags_reg"]
        yr = int(g["hw_reg"][:4])
        added.append({
            "ags": ags, "election_date": g["hw_reg"], "runoff_date": g["sw_reg"],
            "election_type": "Oberbürgermeisterwahl" if g["flag_reg"] == "HaOB" else "Bürgermeisterwahl",
            "register_stand": g["stand"], "register_ags": g["ags_reg"], "register_page": g["page"],
            "n_issues": len({x["stand"] for x in gs}),
            "in_crosswalk": "year" if yr in cw[ags] else ("year-1" if yr - 1 in cw[ags] else "no"),
        })
    if GERDA_INPUT != ST_BEFORE:
        raise SystemExit("--write-pins needs the unpatched input (do not combine with --after)")
    guard_pin_loss(META / "st_b702_added_elections.csv", added, ("ags", "election_date"))
    write(META / "st_b702_added_elections.csv", added)
    print(f"  pins: {len(added)} added elections; crosswalk: "
          f"{dict(Counter(a['in_crosswalk'] for a in added))}")

    fills = {}
    by_el = defaultdict(list)
    for r in cmp_rows:
        if r.get("ags_gerda"):
            by_el[(r["ags_gerda"], r["hw_gerda"])].append(r)
    for (ags, d), rs in sorted(by_el.items()):
        if any(r["same_last"] is not True for r in rs):
            continue  # only where the register names GERDA's winner
        last = rs[0]["last_gerda"]
        parties = {PARTY_OK.get(r["party_reg"]) for r in rs if r["party_reg"]}
        if not rs[0]["party_gerda"] and parties and None not in parties and len(parties) == 1:
            fills[("party", ags, d)] = {"kind": "party", "ags": ags, "election_date": d,
                                        "winner_last_name": last, "old_value": "",
                                        "new_value": parties.pop(),
                                        "issues": ";".join(sorted({r["stand"][:4] for r in rs}))}
        years = {r["by_reg"] for r in rs if r["by_reg"]}
        if not rs[0]["by_gerda"] and len(years) == 1:
            fills[("birth_year", ags, d)] = {"kind": "birth_year", "origin": "fill", "ags": ags,
                                             "election_date": d, "winner_last_name": last,
                                             "old_value": "", "new_value": years.pop(),
                                             "issues": ";".join(sorted({r["stand"][:4] for r in rs}))}
    party = [v for v in fills.values() if v["kind"] == "party"]
    by_pins = [v for v in fills.values() if v["kind"] == "birth_year"]
    by_path = B702 / "st_b702_birth_year_pins_restricted.csv"
    if by_path.exists():
        by_pins += [r for r in csv.DictReader(by_path.open(encoding="utf-8")) if r["origin"] == "correction"]
    guard_pin_loss(META / "st_b702_fills.csv", party, ("kind", "ags", "election_date"))
    guard_pin_loss(by_path, by_pins, ("kind", "ags", "election_date"))
    write(META / "st_b702_fills.csv", party)
    write(by_path, by_pins)
    print(f"  pins: {len(party)} party fills; {len(by_pins)} birth-year pins (restricted)")


def is_new(k: str) -> bool:
    return k[2:5] in ("001", "002", "003") or "081" <= k[2:5] <= "091"


def main() -> None:
    st, el = load_gerda()
    cmp_rows, gaps = compare_register(el)
    write(B702 / "st_b702_compare_restricted.csv", cmp_rows)
    write(B702 / "st_b702_gaps_restricted.csv", gaps)
    m = [r for r in cmp_rows if r.get("ags_gerda")]
    print(f"register Bürgermeister records: {len(cmp_rows)}; matched to a GERDA election: {len(m)}")
    print("  match:", dict(Counter(r["match"] for r in cmp_rows)))
    for f in ("same_last", "same_first", "same_party", "same_by", "runoff_reg_in_gerda", "runoff_gerda_not_reg"):
        print(f"  {f}:", dict(Counter(str(r.get(f, "")) for r in m)))
    distinct = {}
    for g in gaps:
        distinct.setdefault((norm_unit(g["unit_reg"]), g["hw_reg"]), g)
    print(f"register elections GERDA lacks on that date: {len(gaps)} records, {len(distinct)} distinct:",
          dict(Counter(g["class"] for g in distinct.values())))
    pf = {(r["ags_gerda"], r["hw_gerda"]) for r in m if r["party_reg"] and not r["party_gerda"]}
    print(f"distinct elections with a register party and none in GERDA: {len(pf)}")
    if "--write-pins" in __import__("sys").argv:
        write_pins(st, cmp_rows, gaps)
    ann = compare_annex(st)
    write(B702 / "st_b702_annex_compare_restricted.csv", ann)
    by_el = defaultdict(list)
    for a in ann:
        by_el[(a["kreis"], a["election_type"], a["hw_date"])].append(a)
    print("annex elections (votes agree with GERDA / candidates):")
    for k, rs in sorted(by_el.items()):
        e = rs[0]
        ok_hw = sum(1 for a in rs if a["hw_agrees"])
        sw = [a for a in rs if a["votes_sw"]]
        ok_sw = sum(1 for a in sw if a["sw_agrees"] is True)
        print(f"  {k[0]} {k[1][:4]} {k[2]} {e['sw_date'] or '':10} in GERDA {str(e['in_gerda']):5} "
              f"HW {ok_hw}/{len(rs)}  SW {ok_sw}/{len(sw)}  valid annex {e['valid_hw']} GERDA {e.get('g_valid', '')}"
              f"  decisive-round flag {e.get('g_flag_decisive_round_missing', '')}")


if __name__ == "__main__":
    main()
