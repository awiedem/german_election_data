#!/usr/bin/env python3
"""Stage-0 scraper/parser for Sachsen-Anhalt (ST) mayoral (Bürgermeister) elections.

Source: Landeswahlleiter Sachsen-Anhalt election portal
  https://wahlergebnisse.sachsen-anhalt.de/wahlen/bm{YY}/
The portal carries the direct Bürgermeister-/Oberbürgermeisterwahlen of 2024-2026
only (sections bm24 / bm25 / bm26; older years are not published there). Each
year is organised by Landkreis; per-Gemeinde result print-pages live under
`gem/`. This is a recent-years cross-section, not a full historical series.

Drill-down (static HTML, ISO-8859-1):
  bm{YY}/erg/karte/bm.k{NN}g.name.html  -> per-Kreis list of Gemeinden;
      each <a> text is the Gemeinde name, href = ../gem/bm.{AGS8}.ergtab.frametab.html
  bm{YY}/erg/gem/bm.{AGS8}.ergtab.dr.html  -> the result PRINT page:
    Table 0: "Bürgermeister: <Name>" / "Oberbürgermeister: <Name>" (the winner)
    Table 1: "Gegenstand der Nachweisung | Hauptwahl am <date> | Stichwahl am <date>"
             then Zahl/% columns; rows Wahlberechtigte / Wähler / Ungültige /
             Gültige, then "davon entfielen auf:" and the candidates
             "N. Name (Partei)" with Hauptwahl Zahl/% and (if they advanced)
             Stichwahl Zahl/%. Candidates with no party are Einzelbewerber.

Office: kreisfreie Städte (Dessau-Roßlau 15001000, Halle 15002000,
Magdeburg 15003000) -> Oberbürgermeisterwahl; otherwise Bürgermeisterwahl.
(No OB election fell in the 2024-2026 window, but the classifier is robust.)

Output (candidate-level long, one row per candidate per round; BB-compatible):
  data/mayoral_elections/raw/sachsen_anhalt/st_bm_parsed.csv
Raw HTML pages are cached under html_cache/.

Run:  python3 code/mayoral_elections/00_st_scrape.py
The R Stage-1 scripts (01_mayoral_unharm.R, 01b_mayoral_candidates.R) read the CSV.
"""

import csv
import os
import re
import sys
import time
from urllib.parse import urljoin

import requests
from bs4 import BeautifulSoup

HERE = os.path.dirname(os.path.abspath(__file__))
ROOT = os.path.dirname(os.path.dirname(HERE))
RAW_DIR = os.path.join(ROOT, "data", "mayoral_elections", "raw", "sachsen_anhalt")
OUT = os.path.join(RAW_DIR, "st_bm_parsed.csv")
HTML_DIR = os.path.join(RAW_DIR, "html_cache")

BASE = "https://wahlergebnisse.sachsen-anhalt.de/wahlen"
YEARS = ["24", "25", "26"]
KREISE = ["1", "2", "3", "81", "82", "83", "84", "85", "86", "87", "88", "89", "90", "91"]
STATE, STATE_NAME = "15", "Sachsen-Anhalt"
KREISFREIE = {"15001000", "15002000", "15003000"}

HEADERS = {"User-Agent": "Mozilla/5.0 (research; GERDA mayoral data collection)"}
DATE_RE = re.compile(r"(\d{2}\.\d{2}\.\d{4})")


def fetch(url, cache_name):
    path = os.path.join(HTML_DIR, cache_name)
    if os.path.exists(path) and os.path.getsize(path) > 200:
        with open(path, "r", encoding="utf-8") as fh:
            return fh.read()
    try:
        r = requests.get(url, headers=HEADERS, timeout=30)
    except Exception as e:
        sys.stderr.write(f"  ! fetch failed {url}: {e}\n")
        return None
    if r.status_code != 200 or "404 Not Found" in r.text[:300]:
        return None
    r.encoding = "ISO-8859-1"          # the ST portal is Latin-1
    txt = r.text
    os.makedirs(HTML_DIR, exist_ok=True)
    with open(path, "w", encoding="utf-8") as fh:
        fh.write(txt)
    time.sleep(0.12)
    return txt


def iso(de):
    m = re.match(r"(\d{2})\.(\d{2})\.(\d{4})", de or "")
    return f"{m.group(3)}-{m.group(2)}-{m.group(1)}" if m else ""


def to_int(s):
    if s is None:
        return None
    s = re.sub(r"\s", "", s).replace("\xa0", "")
    return int(s) if s.isdigit() else None        # "." (null marker) -> None


def to_share(s):
    if not s:
        return None
    s = re.sub(r"\s", "", s).replace("\xa0", "")
    if s in ("", ".", "-"):
        return None
    s = s.replace(",", ".")
    try:
        return float(s) / 100.0
    except ValueError:
        return None


def split_name_party(raw):
    """'2. Beyer-Kögel, Christin (CDU)' -> (name, party). No party -> Einzelbewerber."""
    s = re.sub(r"^\s*\d+\.\s*", "", raw).strip()          # drop the "N." rank prefix
    m = re.search(r"\(([^)]+)\)\s*$", s)
    party = m.group(1).strip() if m else None
    name = re.sub(r"\s*\([^)]+\)\s*$", "", s).strip()
    return name, party


def split_name(full):
    full = re.sub(r"\s+", " ", full).strip()
    if "," in full:
        last, first = [x.strip() for x in full.split(",", 1)]
    else:
        last, first = full, ""
    return full, last, first


def cells(tr):
    return [re.sub(r"\s+", " ", td.get_text(" ", strip=True)).strip()
            for td in tr.find_all(["td", "th"])]


def collect_gemeinden():
    """Return list of (year, ags8, name, result_url)."""
    items = []
    for yy in YEARS:
        for nn in KREISE:
            url = f"{BASE}/bm{yy}/erg/karte/bm.k{nn}g.name.html"
            html = fetch(url, f"ST_bm{yy}_k{nn}g_name.html")
            if html is None:
                continue
            soup = BeautifulSoup(html, "html.parser")
            for a in soup.find_all("a", href=True):
                m = re.search(r"gem/bm\.(\d{8})\.ergtab", a["href"])
                if not m:
                    continue
                ags = m.group(1)
                name = re.sub(r"\s+", " ", a.get_text(" ", strip=True)).strip()
                res = f"{BASE}/bm{yy}/erg/gem/bm.{ags}.ergtab.dr.html"
                items.append((yy, ags, name, res))
    # dedupe (a Gemeinde appears once per year)
    seen, out = set(), []
    for it in items:
        key = (it[0], it[1])
        if key not in seen:
            seen.add(key)
            out.append(it)
    return out


def parse_result(html, yy, ags, name):
    soup = BeautifulSoup(html, "html.parser")
    tables = soup.find_all("table")
    winner = None
    office = "Bürgermeisterwahl"
    for tbl in tables:                       # Table 0: "(Ober)Bürgermeister[ (...)]: <Name>"
        txt = tbl.get_text(" ", strip=True)
        m = re.match(r"\s*(Ober)?[Bb]ürgermeister(?:in)?\s*(?:\([^)]*\))?\s*:\s*(.+)", txt)
        if m:
            office = "Oberbürgermeisterwahl" if m.group(1) else "Bürgermeisterwahl"
            winner = re.sub(r"\s+", " ", m.group(2)).strip()
            break
    if ags in KREISFREIE:
        office = "Oberbürgermeisterwahl"

    # Find the main results table (the one with "Gegenstand der Nachweisung").
    # Normalise whitespace first — some pages use non-breaking spaces (\xa0)
    # between the words, which a literal substring check would miss.
    main = None
    for tbl in tables:
        if "Gegenstand der Nachweisung" in re.sub(r"\s+", " ", tbl.get_text(" ")):
            main = tbl
            break
    if main is None:
        return []

    rows = [cells(tr) for tr in main.find_all("tr")]
    hw_date = sw_date = None
    # Header columns are "Hauptwahl am <date>" + "Stichwahl am <date>" when there
    # is a runoff, but just "Wahl am <date>" for single-round elections.
    for r in rows:
        joined = " ".join(r)
        for m in re.finditer(r"(\w*wahl) am\s*(\d{2}\.\d{2}\.\d{4})", joined, re.I):
            if "stich" in m.group(1).lower():
                sw_date = m.group(2)
            else:                               # "Hauptwahl am" or "Wahl am"
                hw_date = m.group(2)
    if not hw_date:
        return []
    has_sw = sw_date is not None

    def nums(r):
        """Numeric cells after the leading label cell."""
        return r[1:]

    agg = {"hw": {}, "sw": {}}
    cands = []
    in_cands = False                            # candidate rows follow "davon entfielen auf:"
    for r in rows:
        if not r:
            continue
        label = r[0]
        n = nums(r)
        if "davon entfielen auf" in label.lower():
            in_cands = True
            continue
        if not in_cands:
            key = None
            if label.startswith("Wahlberechtigte"):
                key = "eligible"
            elif label.startswith("Wähler"):
                key = "voters"
            elif label.startswith("Ungültige"):
                key = "invalid"
            elif label.startswith("Gültige"):
                key = "valid"
            if key:
                agg["hw"][key] = to_int(n[0]) if len(n) > 0 else None
                if has_sw:
                    agg["sw"][key] = to_int(n[2]) if len(n) > 2 else None
            continue
        # candidate row — uncontested single candidates have no "N." rank prefix,
        # so detect by position (after "davon entfielen auf:") + a numeric first cell.
        if n and to_int(n[0]) is not None:
            cname, party = split_name_party(label)
            hw_v = to_int(n[0]) if len(n) > 0 else None
            hw_s = to_share(n[1]) if len(n) > 1 else None
            sw_v = to_int(n[2]) if len(n) > 2 else None
            sw_s = to_share(n[3]) if len(n) > 3 else None
            cands.append({"name": cname, "party": party,
                          "hw_v": hw_v, "hw_s": hw_s, "sw_v": sw_v, "sw_s": sw_s})
    if not cands:
        return []

    out = []
    rounds = [("hauptwahl", hw_date, "hw", "hw_v", "hw_s")]
    if has_sw:
        rounds.append(("stichwahl", sw_date, "sw", "sw_v", "sw_s"))
    for rnd, date, ak, vk, sk in rounds:
        ev = agg[ak].get("eligible")
        nv = agg[ak].get("voters")
        turnout = (nv / ev) if (ev and nv) else ""
        for c in cands:
            if c[vk] is None and rnd == "stichwahl":
                continue                       # candidate did not advance to SW
            full, last, first = split_name(c["name"])
            out.append({
                "ags": ags, "ags_name": name, "state": STATE, "state_name": STATE_NAME,
                "election_year": iso(date)[:4], "election_date": iso(date),
                "election_type": office, "round": rnd,
                "eligible_voters": ev if ev is not None else "",
                "number_voters": nv if nv is not None else "",
                "valid_votes": agg[ak].get("valid") if agg[ak].get("valid") is not None else "",
                "invalid_votes": agg[ak].get("invalid") if agg[ak].get("invalid") is not None else "",
                "turnout": turnout,
                "candidate_name": full, "candidate_last_name": last,
                "candidate_first_name": first, "candidate_party": c["party"] or "",
                "candidate_votes": c[vk] if c[vk] is not None else "",
                "candidate_voteshare": c[sk] if c[sk] is not None else "",
                "source_url": f"bm{yy}/erg/gem/bm.{ags}.ergtab.dr.html",
            })
    return out


FIELDS = ["ags", "ags_name", "state", "state_name", "election_year",
          "election_date", "election_type", "round", "eligible_voters",
          "number_voters", "valid_votes", "invalid_votes", "turnout",
          "candidate_name", "candidate_last_name", "candidate_first_name",
          "candidate_party", "candidate_votes", "candidate_voteshare", "source_url"]


def main():
    os.makedirs(RAW_DIR, exist_ok=True)
    sys.stderr.write("=== ST Bürgermeisterwahl scraper ===\n")
    gems = collect_gemeinden()
    sys.stderr.write(f"  {len(gems)} Gemeinde-elections across bm{'/'.join(YEARS)}\n")

    all_rows, failed = [], []
    for yy, ags, name, url in gems:
        html = fetch(url, f"ST_bm{yy}_{ags}.html")
        if html is None:
            failed.append((yy, ags, name))
            continue
        rows = parse_result(html, yy, ags, name)
        if not rows:
            failed.append((yy, ags, name))
            continue
        all_rows.extend(rows)

    n_ags = len(set(r["ags"] for r in all_rows))
    n_hw = len(set((r["ags"], r["election_date"]) for r in all_rows if r["round"] == "hauptwahl"))
    n_sw = len(set((r["ags"], r["election_date"]) for r in all_rows if r["round"] == "stichwahl"))
    n_ob = len(set(r["ags"] for r in all_rows if r["election_type"] == "Oberbürgermeisterwahl"))
    sys.stderr.write(
        f"  candidate rows: {len(all_rows)} | Gemeinden: {n_ags} | "
        f"HW: {n_hw} | SW: {n_sw} | OB: {n_ob}\n")
    if failed:
        sys.stderr.write(f"  WARNING: {len(failed)} result pages failed to parse:\n")
        for yy, a, nm in failed[:12]:
            sys.stderr.write(f"     bm{yy} {a} {nm}\n")

    with open(OUT, "w", newline="", encoding="utf-8") as fh:
        w = csv.DictWriter(fh, fieldnames=FIELDS)
        w.writeheader()
        w.writerows(all_rows)
    sys.stderr.write(f"  Wrote {len(all_rows)} rows -> {OUT}\n")


if __name__ == "__main__":
    main()
