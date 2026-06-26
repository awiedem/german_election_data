#!/usr/bin/env python3
"""Stage-0 scraper/parser for Brandenburg (BB) mayoral (Bürgermeister) elections.

Source: Landeswahlleiter Brandenburg election portal
  https://wahlen.brandenburg.de/wahlen/de/kommunalwahlen/ergebnisse/buergermeisterwahlen/
The portal publishes, for each Landkreis / kreisfreie Stadt, the *most recent*
direct Bürgermeister-/Oberbürgermeisterwahl of each amtsfreie Gemeinde / Stadt
(so dates span ~2019-2026; this is a current-mayor cross-section, not a full
historical series — pre-2018 is not on the portal).

Structure (mirrors the existing BB *Landrat* scraper, code/landrat_elections/00_bb_scrape.R):
  buergermeisterwahlen/                          -> index, links 18 Kreis pages
  buergermeisterwahlen/ergebnisse/~120XX000      -> per-Kreis overview, links
      result pages ~h_{DDMMYYYY}_{AGS8}  (Hauptwahl)
      result pages ~s_{DDMMYYYY}_{AGS8}  (Stichwahl)
  Each result page has 3 tables:
    Table 0: Wahltag, Wahlart, Landkreis, "<typ>" (amtsfr. Stadt / amtsfr.
             Gemeinde / kreisfr. Stadt) + Gemeinde name
    Table 1: Wahlberechtigte / Wähler / Ungültige / Gültige Stimmen
    Table 2: Lfd Nr | Name | Wahlvorschlagsträger (party) | Stimmen (Anzahl, %)

Scope: amtsfreie Gemeinden/Städte + kreisfreie Städte (8-digit AGS) = the
directly-elected hauptamtliche Bürgermeister/Oberbürgermeister. The amtsangehörige
Gemeinden (ehrenamtliche Bürgermeister, 12-digit portal keys, all on the
Kommunalwahl day) are NOT scraped here. Unlike BW, BB publishes party + all
candidates, so this populates the full candidate schema.

Office: "kreisfr. Stadt" -> Oberbürgermeisterwahl; otherwise Bürgermeisterwahl.

Output (candidate-level long, one row per candidate per round; MV/TH/BW-compatible):
  data/mayoral_elections/raw/brandenburg/bb_bm_parsed.csv
Raw HTML pages are cached under the same directory.

Run:  python3 code/mayoral_elections/00_bb_scrape.py
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
RAW_DIR = os.path.join(ROOT, "data", "mayoral_elections", "raw", "brandenburg")
OUT = os.path.join(RAW_DIR, "bb_bm_parsed.csv")
HTML_DIR = os.path.join(RAW_DIR, "html_cache")

BASE = "https://wahlen.brandenburg.de"
INDEX = BASE + "/wahlen/de/kommunalwahlen/ergebnisse/buergermeisterwahlen/"
STATE, STATE_NAME = "12", "Brandenburg"

# The 4 kreisfreie Städte (Oberbürgermeister) — also pinned by AGS as a backstop.
KREISFREIE = {"12051000", "12052000", "12053000", "12054000"}

HEADERS = {"User-Agent": "Mozilla/5.0 (research; GERDA mayoral data collection)"}
# ~h/~s _ DDMMYYYY _ AGS. Amtsfreie Gemeinden/Städte use the plain 8-digit AGS;
# the kreisfreie Städte use a 12-digit form (8-digit AGS + "0000"). Amtsangehörige
# Gemeinden (ehrenamtliche BM) also use 12-digit (Amt+Gemeinde) keys — those are
# scoped out in parse_result (their first 8 digits are not a real Gemeinde AGS).
RESULT_RE = re.compile(r"^~([hs])_(\d{8})_(\d{8}|\d{12})$")


def fetch(url, cache_name=None):
    """GET a URL, caching to HTML_DIR. Returns decoded text or None."""
    if cache_name:
        path = os.path.join(HTML_DIR, cache_name)
        if os.path.exists(path) and os.path.getsize(path) > 800:
            with open(path, "r", encoding="utf-8") as fh:
                return fh.read()
    try:
        r = requests.get(url, headers=HEADERS, timeout=30)
    except Exception as e:
        sys.stderr.write(f"  ! fetch failed {url}: {e}\n")
        return None
    if r.status_code != 200:
        return None
    r.encoding = r.apparent_encoding or "utf-8"
    txt = r.text
    if cache_name:
        os.makedirs(HTML_DIR, exist_ok=True)
        with open(os.path.join(HTML_DIR, cache_name), "w", encoding="utf-8") as fh:
            fh.write(txt)
        time.sleep(0.15)  # be nice to the server
    return txt


def kreis_pages(index_html):
    """Extract the per-Kreis overview-page URLs from the index."""
    soup = BeautifulSoup(index_html, "html.parser")
    urls = set()
    for a in soup.find_all("a", href=True):
        href = a["href"]
        if re.search(r"/buergermeisterwahlen/ergebnisse/~12\d{6}$", href):
            urls.add(urljoin(INDEX, href))
    return sorted(urls)


def result_links(kreis_html, kreis_url):
    """Extract ~h_/~s_ result-page links with an 8-digit AGS (amtsfreie + Städte)."""
    soup = BeautifulSoup(kreis_html, "html.parser")
    out = {}
    for a in soup.find_all("a", href=True):
        href = a["href"].strip()
        m = RESULT_RE.match(href)
        if not m:
            continue
        ags_raw = m.group(3)
        # Keep only the hauptamtliche mayors: 8-digit AGS (amtsfreie Gemeinden/
        # Städte) + the kreisfreie Städte (12-digit = AGS8 + "0000"). Skip the
        # ~323 amtsangehörige ehrenamtliche BM (12-digit Amt+Gemeinde keys) so we
        # don't hammer the server fetching pages we then discard.
        if len(ags_raw) == 8 or (len(ags_raw) == 12 and ags_raw[:8] in KREISFREIE):
            # The Kreis page (~12060000) is a "file" under .../ergebnisse/, so the
            # result links resolve as siblings, NOT nested under the Kreis page.
            out[href] = urljoin(kreis_url, href)
    return out  # href-token -> absolute url


def cells(tr):
    return [re.sub(r"\s+", " ", td.get_text(" ", strip=True)).strip()
            for td in tr.find_all(["td", "th"])]


def to_int(s):
    if s is None:
        return None
    s = re.sub(r"[.\s]", "", s).replace("–", "")
    return int(s) if s.isdigit() else None


def to_share(s):
    if not s:
        return None
    s = s.strip().replace("–", "")
    s = s.replace(".", "").replace(",", ".")
    try:
        return float(s) / 100.0
    except ValueError:
        return None


def split_name(full):
    full = re.sub(r"\s+", " ", full).strip()
    if "," in full:
        last, first = [x.strip() for x in full.split(",", 1)]
    else:
        last, first = full, ""
    return full, last, first


def parse_result(html, href):
    """Parse one result page into a list of candidate-row dicts."""
    m = RESULT_RE.match(href)
    sw = m.group(1) == "s"
    dd, mm, yyyy = m.group(2)[0:2], m.group(2)[2:4], m.group(2)[4:8]
    ags_raw = m.group(3)
    if len(ags_raw) == 8:
        ags = ags_raw                      # amtsfreie Gemeinde/Stadt (incl. Cottbus)
    else:                                   # 12-digit
        ags = ags_raw[:8]
        if ags not in KREISFREIE:           # amtsangehörige BM or Kreis aggregate -> skip
            return []
    iso = f"{yyyy}-{mm}-{dd}"
    soup = BeautifulSoup(html, "html.parser")
    tables = soup.find_all("table")

    # The page H1 ("Ergebnis der (Ober)Bürgermeisterwahl in <Name>") is the most
    # robust source of the Gemeinde name (Table 0's type label varies:
    # "amtsfr. Stadt", "kreisfr. Stadt", "Landeshauptstadt", "amtsfr. Gemeinde").
    h1name = None
    h1 = soup.find("h1")
    if h1:
        mh = re.search(r"[Bb]ürgermeisterwahl\s+in\s+(.+)$",
                       re.sub(r"\s+", " ", h1.get_text(" ", strip=True)))
        if mh:
            h1name = mh.group(1).strip()

    meta, summ = {}, {}
    gem_name, gem_typ = None, None
    for tbl in tables:
        for tr in tbl.find_all("tr"):
            c = [x for x in cells(tr) if x != ""]
            if len(c) == 2 and c[0] in ("Wahltag", "Wahlart", "Landkreis"):
                meta[c[0]] = c[1]
            elif len(c) == 2 and re.search(r"stadt|gemeinde", c[0], re.I):
                gem_typ, gem_name = c[0], c[1]  # e.g. "amtsfr. Stadt" -> "Eberswalde"
            elif len(c) >= 2 and c[0].startswith("Wahlberechtigte"):
                summ["eligible"] = to_int(c[1])
            elif len(c) >= 2 and c[0].startswith("Wähler"):
                summ["voters"] = to_int(c[1])
            elif len(c) >= 2 and c[0].startswith("Ungültige"):
                summ["invalid"] = to_int(c[1])
            elif len(c) >= 2 and c[0].startswith("Gültige"):
                summ["valid"] = to_int(c[1])

    # Candidate table: rows starting with a Lfd-Nr integer + name + party + votes.
    cand = []
    for tbl in tables:
        for tr in tbl.find_all("tr"):
            c = [x for x in cells(tr) if x != ""]
            if len(c) >= 4 and c[0].isdigit() and re.search(r"[A-Za-zÄÖÜäöü]", c[1]):
                # [lfd, name, party, votes, (pct)]
                votes = to_int(c[-2]) if to_share(c[-1]) is not None else to_int(c[-1])
                share = to_share(c[-1]) if to_share(c[-1]) is not None else None
                party = c[2] if len(c) >= 5 else (c[2] if len(c) == 4 else None)
                # When pct present: c = [lfd, name, party, votes, pct]
                if len(c) >= 5:
                    name, party, votes, share = c[1], c[2], to_int(c[3]), to_share(c[4])
                else:  # [lfd, name, party, votes]  (no pct, rare)
                    name, party, votes, share = c[1], c[2], to_int(c[3]), None
                full, last, first = split_name(name)
                cand.append({"name": full, "last": last, "first": first,
                             "party": party, "votes": votes, "share": share,
                             "rank": int(c[0])})
    if not cand:
        return []

    etype = ("Oberbürgermeisterwahl"
             if (ags in KREISFREIE or (gem_typ and "kreisfr" in gem_typ.lower()))
             else "Bürgermeisterwahl")
    ev, nv = summ.get("eligible"), summ.get("voters")
    turnout = (nv / ev) if (ev and nv) else ""
    rows = []
    for k in cand:
        rows.append({
            "ags": ags,
            "ags_name": gem_name or h1name or "",
            "state": STATE, "state_name": STATE_NAME,
            "election_year": yyyy, "election_date": iso,
            "election_type": etype,
            "round": "stichwahl" if sw else "hauptwahl",
            "eligible_voters": ev if ev is not None else "",
            "number_voters": nv if nv is not None else "",
            "valid_votes": summ.get("valid") if summ.get("valid") is not None else "",
            "invalid_votes": summ.get("invalid") if summ.get("invalid") is not None else "",
            "turnout": turnout,
            "candidate_name": k["name"], "candidate_last_name": k["last"],
            "candidate_first_name": k["first"], "candidate_party": k["party"] or "",
            "candidate_votes": k["votes"] if k["votes"] is not None else "",
            "candidate_voteshare": k["share"] if k["share"] is not None else "",
            "source_url": href,
        })
    return rows


FIELDS = ["ags", "ags_name", "state", "state_name", "election_year",
          "election_date", "election_type", "round", "eligible_voters",
          "number_voters", "valid_votes", "invalid_votes", "turnout",
          "candidate_name", "candidate_last_name", "candidate_first_name",
          "candidate_party", "candidate_votes", "candidate_voteshare", "source_url"]


def main():
    os.makedirs(RAW_DIR, exist_ok=True)
    sys.stderr.write("=== BB Bürgermeisterwahl scraper ===\n")
    idx = fetch(INDEX, "BB_index.html")
    if idx is None:
        raise SystemExit("Could not fetch index page")
    kpages = kreis_pages(idx)
    sys.stderr.write(f"  {len(kpages)} Kreis/Stadt overview pages\n")

    links = {}
    for ku in kpages:
        kid = re.search(r"~(12\d{6})$", ku).group(1)
        kh = fetch(ku, f"BB_kreis_{kid}.html")
        if kh is None:
            continue
        links.update(result_links(kh, ku))
    sys.stderr.write(f"  {len(links)} result pages (8-digit AGS HW/SW)\n")

    all_rows, failed = [], []
    for href, url in sorted(links.items()):
        h = fetch(url, f"BB_{href[1:]}.html")
        if h is None:
            failed.append(href)
            continue
        rows = parse_result(h, href)
        if not rows:
            failed.append(href)
            continue
        all_rows.extend(rows)

    # diagnostics
    n_ags = len(set(r["ags"] for r in all_rows))
    n_hw = len(set(r["ags"] for r in all_rows if r["round"] == "hauptwahl"))
    n_sw = len(set((r["ags"], r["election_date"]) for r in all_rows if r["round"] == "stichwahl"))
    n_ob = len(set(r["ags"] for r in all_rows if r["election_type"] == "Oberbürgermeisterwahl"))
    sys.stderr.write(
        f"  candidate rows: {len(all_rows)} | Gemeinden: {n_ags} | "
        f"HW: {n_hw} | SW: {n_sw} | OB Gemeinden: {n_ob}\n")
    if failed:
        sys.stderr.write(f"  WARNING: {len(failed)} result pages failed to parse:\n")
        for f in failed[:12]:
            sys.stderr.write(f"     {f}\n")

    with open(OUT, "w", newline="", encoding="utf-8") as fh:
        w = csv.DictWriter(fh, fieldnames=FIELDS)
        w.writeheader()
        w.writerows(all_rows)
    sys.stderr.write(f"  Wrote {len(all_rows)} rows -> {OUT}\n")


if __name__ == "__main__":
    main()
