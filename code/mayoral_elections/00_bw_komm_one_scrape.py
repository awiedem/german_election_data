#!/usr/bin/env python3
"""Stage-0 scraper: full candidate-level Baden-Württemberg Bürgermeister- und
Oberbürgermeisterwahl results from Komm.ONE's votemanager portal.

Komm.ONE (BW's merged municipal IT provider) publishes mayoral results per
municipality at wahlergebnisse.komm.one as a votemanager presentation with a
stable, unauthenticated JSON API. Unlike the StaLA report (winner-only, no
party, most-recent election per Gemeinde @31.12.2024 -> bw_parsed.csv), Komm.ONE
gives ALL candidates + votes + full turnout + Hauptwahl & Stichwahl, for the
~54% of municipalities digitized since ~2021. Stages 01/01b merge this with the
StaLA file as a hybrid (Komm.ONE where available, StaLA winner-only fallback).

Discovery / URL scheme (see memory komm-one-bw-mayoral-source):
  ROOT = https://wahlergebnisse.komm.one/lb/produktion
  per-muni registry : ROOT/{AGS8}/api/termine.json -> termine[]{date,name,url}
  election folder E : ROOT/wahltermin-{YYYYMMDD}/{AGS8}
    E/daten/api/termin.json -> wahleintraege[]{wahl.id, wahl.titel,
                               gebiet_link.id, stimmentyp.id}
    result (city level): E/daten/api/wahl_{id}/ergebnis_{gebiet_link.id}_{stimmentyp}.json
      -> Komponente.tabelle.zeilen[]      = candidates (label.labelKurz, zahl, prozent)
         Komponente.info.tabelle.zeilen[] = Wahlberechtigte/Wähler/ungültig/gültig

A wahltermin folder holds the whole CYCLE: Hauptwahl + Stichwahl/Neuwahl are
separate wahleintraege (e.g. Eppelheim 2025: wahl 8331 HW + 8577 Stichwahl). The
correct per-round DATE comes from the per-muni termine.json entry (the folder
date can list both, e.g. "23.03.2025 / 06.04.2025"). Round = stichwahl iff the
title contains "Stichwahl" or "Neuwahl" (BW convention: both -> stichwahl).

Output: data/mayoral_elections/raw/baden_wuerttemberg/bw_komm_one_parsed.csv
Re-run with:  python3 code/mayoral_elections/00_bw_komm_one_scrape.py
JSON responses are cached under komm_one_cache/ (gitignored).
"""
import csv
import json
import os
import re
import sys
import time
import urllib.request
import urllib.error
from concurrent.futures import ThreadPoolExecutor
from threading import Lock

HERE = os.path.dirname(os.path.abspath(__file__))
PROJECT = os.path.abspath(os.path.join(HERE, "..", ".."))
RAW_DIR = os.path.join(PROJECT, "data", "mayoral_elections", "raw", "baden_wuerttemberg")
CACHE_DIR = os.path.join(RAW_DIR, "komm_one_cache")
STALA_CSV = os.path.join(RAW_DIR, "bw_parsed.csv")
OUT = os.path.join(RAW_DIR, "bw_komm_one_parsed.csv")

ROOT = "https://wahlergebnisse.komm.one/lb/produktion"
UA = "GERDA-research/1.0 (academic; german-elections.com)"
# Labels that are NOT named candidates (residual write-in / aggregate rows).
NON_CANDIDATE = {"freie zeile", "übrige", "uebrige", "sonstige", "übrige bewerber"}
# A wahleintrag (within a Bürgermeister/Oberbürgermeister election folder) is a
# mayoral round if its title carries one of these keywords. The Hauptwahl says
# "(Ober)Bürgermeister..."; runoffs are often just "Stichwahl ..."/"Neuwahl ..."/
# "... Wahlgang". Used to keep every round of the cycle (and exclude any
# simultaneous Bundes-/Landtags-/Europa-/Kommunalwahl in a combined folder).
MAYORAL_ROUND = re.compile(r"ürgermeister|stichwahl|neuwahl|wahlgang", re.I)

os.makedirs(CACHE_DIR, exist_ok=True)
_print_lock = Lock()


# --------------------------------------------------------------------------- IO
def fetch_json(url, cache_key, retries=3):
    """GET a JSON URL with on-disk caching. Returns dict or None (404/empty)."""
    cpath = os.path.join(CACHE_DIR, cache_key)
    if os.path.exists(cpath):
        with open(cpath, "r", encoding="utf-8") as fh:
            txt = fh.read()
        return json.loads(txt) if txt else None
    last = None
    for attempt in range(retries):
        try:
            req = urllib.request.Request(url, headers={"User-Agent": UA})
            with urllib.request.urlopen(req, timeout=30) as resp:
                raw = resp.read().decode("utf-8")
            data = json.loads(raw)
            with open(cpath, "w", encoding="utf-8") as fh:
                fh.write(raw)
            return data
        except urllib.error.HTTPError as e:
            if e.code == 404:
                with open(cpath, "w", encoding="utf-8") as fh:
                    fh.write("")          # cache the negative result
                return None
            last = e
        except Exception as e:
            last = e
        time.sleep(0.6 * (attempt + 1))
    with _print_lock:
        print(f"  WARN fetch failed: {url} ({last})", file=sys.stderr)
    return None


# ----------------------------------------------------------------------- parse
def g_int(s):
    """German integer: '2.288' -> 2288 ; '' / '.' -> None."""
    if s is None:
        return None
    s = str(s).strip().replace(".", "").replace(" ", "")
    return int(s) if re.fullmatch(r"\d+", s) else None


def round_of(title):
    """BW second-round markers all map to 'stichwahl': Stichwahl, Neuwahl (the
    pre-2023 term), and the "2. Wahlgang" wording (e.g. Hagnau 2003). The "2." in
    a date like '04.02.2024' or '12. Februar' is NOT matched (we require the
    'Wahlgang' phrase)."""
    t = (title or "").lower()
    if ("stichwahl" in t or "neuwahl" in t
            or "2. wahlgang" in t or "2.wahlgang" in t
            or "zweiter wahlgang" in t or "ii. wahlgang" in t):
        return "stichwahl"
    return "hauptwahl"


def split_name(full):
    """'Matthias Kutsch' -> ('Kutsch','Matthias'); keeps multi-token first names.
    Strips a leading academic title for the name parts (kept in candidate_name)."""
    full = (full or "").strip()
    toks = full.split()
    core = [t for t in toks if not re.fullmatch(r"(Dr\.?|Prof\.?|Dipl\.?-?\w*\.?)", t)]
    if not core:
        return "", ""
    last = core[-1]
    first = " ".join(core[:-1])
    return last, first


def folder_date(url):
    """'../wahltermin-20250323/08226018/praesentation/' -> '20250323'."""
    m = re.search(r"wahltermin-(\d{8})", url or "")
    return m.group(1) if m else None


def iso(ddmmyyyy):
    m = re.fullmatch(r"(\d{2})\.(\d{2})\.(\d{4})", (ddmmyyyy or "").strip())
    return f"{m.group(3)}-{m.group(2)}-{m.group(1)}" if m else None


def parse_ergebnis(d):
    """Komponente -> (candidates[(name,votes)], turnout dict). None if malformed."""
    try:
        komp = d["Komponente"]
        cand_rows = komp["tabelle"]["zeilen"]
        info_rows = komp["info"]["tabelle"]["zeilen"]
    except (KeyError, TypeError):
        return None
    candidates = []
    for z in cand_rows:
        name = (z.get("label", {}) or {}).get("labelKurz", "").strip()
        if not name or name.lower() in NON_CANDIDATE:
            continue
        candidates.append((name, g_int(z.get("zahl"))))
    turn = {}
    for z in info_rows:
        lbl = (z.get("label", {}) or {}).get("labelKurz", "").lower()
        val = g_int(z.get("zahl"))
        if "wahlberechtigt" in lbl:
            turn["eligible_voters"] = val
        elif "wähler" in lbl or "waehler" in lbl:
            turn["number_voters"] = val
        elif "ungültig" in lbl or "ungueltig" in lbl:
            turn["invalid_votes"] = val
        elif "gültig" in lbl or "gueltig" in lbl:
            turn["valid_votes"] = val
    return candidates, turn


# ------------------------------------------------------------------ per muni
def process_muni(ags, ags_name, ob_set):
    """Return (rows, status) for one municipality. status records coverage so the
    run can report the modern-JSON vs older-html5 split transparently (the older
    votemanager generation has no daten/api JSON -> those folders are skipped and
    the municipality falls back to the StaLA winner-only data in stages 01/01b)."""
    rows = []
    status = {"bm_entry": False, "folders": 0, "modern_folders": 0}
    termine = fetch_json(f"{ROOT}/{ags}/api/termine.json", f"termine_{ags}.json")
    if not termine:
        return rows, status
    bm_entries = [t for t in termine.get("termine", [])
                  if "ürgermeister" in t.get("name", "")]
    if not bm_entries:
        return rows, status
    status["bm_entry"] = True
    etype = "Oberbürgermeisterwahl" if ags in ob_set else "Bürgermeisterwahl"

    # Group entries by election folder; within a folder map round -> real date.
    folders = {}
    for t in bm_entries:
        fd = folder_date(t.get("url", ""))
        if not fd:
            continue
        folders.setdefault(fd, {})[round_of(t.get("name"))] = iso(t.get("date"))
    status["folders"] = len(folders)

    for fd, round_dates in folders.items():
        termin = fetch_json(
            f"{ROOT}/wahltermin-{fd}/{ags}/daten/api/termin.json",
            f"termin_{ags}_{fd}.json")
        if not termin:
            continue          # older html5 generation (no JSON API) -> StaLA fallback
        status["modern_folders"] += 1
        # Date source: the folder's datum_string lists the cycle's dates in
        # chronological order (e.g. "07.12.2025 / 21.12.2025"). We assign the
        # Hauptwahl the earliest and the Stichwahl the latest, using the reliable
        # wahleintrag *title* for the round. This is robust against mislabeled
        # per-muni termine.json names (e.g. Oberkochen's Hauptwahl entry is named
        # "Stichwahl Bürgermeister 21.12.2025"). Fall back to termine.json dates
        # only if datum_string is empty.
        folder_dates = sorted(d for d in
                              {iso(x.strip()) for x in str(termin.get("datum_string", "")).split("/")}
                              if d)
        for we in termin.get("wahleintraege", []):
            wahl = we.get("wahl", {})
            wid = wahl.get("id")
            title = wahl.get("titel", "")
            gid = (we.get("gebiet_link", {}) or {}).get("id")
            st = (we.get("stimmentyp", {}) or {}).get("id", 0)
            # Keep the mayoral rounds of this folder. The Hauptwahl title contains
            # "Bürgermeister"/"Oberbürgermeister", but the runoff is often titled just
            # "Stichwahl - <Gemeinde>" / "Neuwahl - <Gemeinde>" / "... 2. Wahlgang"
            # WITHOUT that word — so match the round keywords too, else those decisive
            # rounds get dropped and the cycle looks (wrongly) incomplete. A combined
            # folder's other elections (Bundes-/Landtags-/Europa-/Kommunalwahl) have
            # none of these keywords and are correctly excluded; BW has no other
            # directly-elected office with a Stichwahl.
            if (wid is None or gid is None or
                    not MAYORAL_ROUND.search(title)):
                continue
            rnd = round_of(title)
            if folder_dates:
                date = folder_dates[-1] if rnd == "stichwahl" else folder_dates[0]
            else:
                date = round_dates.get(rnd)
            erg = fetch_json(
                f"{ROOT}/wahltermin-{fd}/{ags}/daten/api/wahl_{wid}/"
                f"ergebnis_{gid}_{st}.json",
                f"erg_{ags}_{fd}_{wid}_{gid}_{st}.json")
            if not erg:
                continue
            parsed = parse_ergebnis(erg)
            if parsed is None:
                continue
            cands, turn = parsed
            valid = turn.get("valid_votes")
            n = len([c for c in cands if c[1] is not None])
            # rank by votes (desc); ties -> stable by listed order
            order = sorted(range(len(cands)),
                           key=lambda i: (cands[i][1] if cands[i][1] is not None else -1),
                           reverse=True)
            rank = {i: r + 1 for r, i in enumerate(order)}
            for i, (name, votes) in enumerate(cands):
                last, first = split_name(name)
                share = (round(votes / valid, 6)
                         if votes is not None and valid else None)
                rows.append({
                    "ags": ags, "ags_name": ags_name,
                    "state": "08", "state_name": "Baden-Württemberg",
                    "election_year": date[:4] if date else "",
                    "election_date": date or "",
                    "election_type": etype, "round": rnd,
                    "eligible_voters": turn.get("eligible_voters", ""),
                    "number_voters": turn.get("number_voters", ""),
                    "valid_votes": valid if valid is not None else "",
                    "invalid_votes": turn.get("invalid_votes", ""),
                    "turnout": (round(turn["number_voters"] / turn["eligible_voters"], 6)
                                if turn.get("number_voters") and turn.get("eligible_voters")
                                else ""),
                    "candidate_name": name,
                    "candidate_last_name": last,
                    "candidate_first_name": first,
                    "candidate_party": "",          # BW: no party
                    "candidate_votes": votes if votes is not None else "",
                    "candidate_voteshare": share if share is not None else "",
                    "candidate_rank": rank[i],
                    "n_candidates": n,
                    "is_winner": (rank[i] == 1),
                    "wahl_id": wid,
                    "source": "komm.one",
                })
    return rows, status


FIELDS = ["ags", "ags_name", "state", "state_name", "election_year", "election_date",
          "election_type", "round", "eligible_voters", "number_voters", "valid_votes",
          "invalid_votes", "turnout", "candidate_name", "candidate_last_name",
          "candidate_first_name", "candidate_party", "candidate_votes",
          "candidate_voteshare", "candidate_rank", "n_candidates", "is_winner",
          "wahl_id", "source"]


def main():
    if not os.path.exists(STALA_CSV):
        sys.exit(f"FATAL: need {STALA_CSV} for the BW municipality + OB-classifier list. "
                 "Run 00_bw_parse.py first.")
    # Municipality list + OB-classifier set, taken from the StaLA file so the
    # Oberbürgermeisterwahl/Bürgermeisterwahl split is identical to bw_parsed.csv.
    ags_name, ob_set, all_ags = {}, set(), []
    with open(STALA_CSV, encoding="utf-8") as fh:
        for r in csv.DictReader(fh):
            a = r["ags"]
            if a not in ags_name:
                ags_name[a] = r["ags_name"]
                all_ags.append(a)
            if r["election_type"] == "Oberbürgermeisterwahl":
                ob_set.add(a)
    print(f"BW municipalities: {len(all_ags)}  (Oberbürgermeister AGS: {len(ob_set)})")

    all_rows, done = [], [0]
    agg = {"bm_entry": 0, "modern": 0, "older_only": 0, "modern_no_results": 0}
    lock = Lock()

    def work(ags):
        rws, st = process_muni(ags, ags_name[ags], ob_set)
        with lock:
            all_rows.extend(rws)
            if st["bm_entry"]:
                agg["bm_entry"] += 1
                if st["modern_folders"] > 0:
                    agg["modern"] += 1
                    if not rws:          # modern JSON shell but empty result table
                        agg["modern_no_results"] += 1
                else:
                    agg["older_only"] += 1
            done[0] += 1
            if done[0] % 100 == 0:
                print(f"  ...{done[0]}/{len(all_ags)} municipalities probed")
        return len(rws)

    with ThreadPoolExecutor(max_workers=8) as ex:
        list(ex.map(work, all_ags))

    all_rows.sort(key=lambda r: (r["ags"], r["election_date"], r["round"],
                                 r["candidate_rank"]))
    with open(OUT, "w", encoding="utf-8", newline="") as fh:
        w = csv.DictWriter(fh, fieldnames=FIELDS)
        w.writeheader()
        w.writerows(all_rows)

    munis = {r["ags"] for r in all_rows}
    elections = {(r["ags"], r["election_date"]) for r in all_rows}
    print(f"\nWrote {len(all_rows)} candidate rows  | "
          f"{len(munis)} municipalities  | {len(elections)} election-rounds")
    print(f"  -> {OUT}")
    print(f"\nCoverage of BW municipalities with a Komm.ONE BM/OB election:")
    print(f"  {agg['bm_entry']} have a mayoral election listed")
    print(f"    - {agg['modern']} in the modern JSON format ({agg['modern'] - agg['modern_no_results']} "
          f"with detailed results -> this file; {agg['modern_no_results']} with an empty "
          f"published result table -> StaLA fallback)")
    print(f"    - {agg['older_only']} only in the older html5 format -> NOT scraped "
          f"(StaLA winner-only fallback in 01/01b)")


if __name__ == "__main__":
    main()
