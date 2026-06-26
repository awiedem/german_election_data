#!/usr/bin/env python3
"""
Stage 0 — Thüringen Bürgermeisterwahlen (all Gemeinden) scraper.

Source: the Thüringer Landesamt für Statistik election database at
  https://wahlen.thueringen.de/datenbank/wahl1/wahl.asp?wahlart=BM&...
which publishes every reported Bürgermeisterwahl of every Thüringen Gemeinde
since 1994 (hauptamtlich + ehrenamtlich). Open data (Datenlizenz Deutschland).

This complements the kreisfreie-Stadt Oberbürgermeister (OB) elections that come
from the raw `thueringen/` Info files (00_th_mayoral_parse / 01_mayoral_unharm) —
the BM database does NOT contain the 6 kreisfreie Städte.

Mechanism (reverse-engineered):
  * The landing page `WAHL.asp?wahlart=BM&wJahr=0000` links to ~1000 individual
    results; we use it to seed the set of (wknr, gemnr) Gemeinden.
  * Each Gemeinde page has a <select name=w_datum> dropdown listing ALL of that
    Gemeinde's election dates (Hauptwahl dates). We enumerate those.
  * Fetching `...&wJahr=0000&...&gemnr=G&w_datum=D` returns that election's page,
    which shows the Hauptwahl results and, stacked below, the Stichwahl block
    ("Stichwahl am DATE ...") when a runoff was held. We parse both rounds.
  * Candidate names are redacted for privacy (§ 50 Abs. 2 ThürKWO); the
    "Wahlvorschlag" column holds the party (CDU, SPD, ...) or an Einzelbewerber
    label / person name. We store it as candidate_party (+ candidate_name when it
    looks like "Last, First").

AGS: 8-digit = "16" + gemnr.zfill(6)  (e.g. gemnr 73002 -> 16073002).

Output: data/mayoral_elections/raw/thueringen_bm/th_bm_scraped.csv (candidate-level
long, one row per candidate per round). Raw HTML is cached under CACHE_DIR.

Run:  python3 code/mayoral_elections/00_th_scrape.py
Re-runs reuse the HTML cache (idempotent). Polite: ~0.4s between requests.
"""

import os
import re
import sys
import csv
import time
import html
import datetime
import subprocess
from bs4 import BeautifulSoup

HERE = os.path.dirname(os.path.abspath(__file__))
ROOT = os.path.abspath(os.path.join(HERE, "..", ".."))
OUT_DIR = os.path.join(ROOT, "data", "mayoral_elections", "raw", "thueringen_bm")
CACHE_DIR = os.path.join(OUT_DIR, "html_cache")
OUT_CSV = os.path.join(OUT_DIR, "th_bm_scraped.csv")
os.makedirs(CACHE_DIR, exist_ok=True)

BASE = "https://wahlen.thueringen.de/datenbank/wahl1/"
UA = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) research-data-collection"
REFERER = BASE + "wahl.asp?wahlart=BM&wJahr=0000"
TODAY = datetime.date(2026, 6, 25)          # skip elections scheduled after this
DELAY = 0.4

# kreisfreie-Stadt gemnr prefixes (handled by the OB raw-file pipeline, not here)
KFS_PREFIX = ("51", "52", "53", "54", "55", "56")

PARTY_RE = re.compile(
    r"^(CDU|SPD|AfD|FDP|F\.D\.P\.|DIE LINKE|Die Linke\.?|PDS|GRÜNE|B90|BÜNDNIS|"
    r"NPD|REP|DSU|DVU|ödp|FREIE WÄHLER|Freie Wähler|FW|pro|Die PARTEI)\b", re.I)


def _curl(url):
    return subprocess.run(
        ["curl", "-s", "--compressed", "--max-time", "45", "-A", UA, "-e", REFERER, url],
        capture_output=True,
    ).stdout


def fetch(url, cache_key, force=False):
    fn = os.path.join(CACHE_DIR, cache_key + ".html")
    if os.path.exists(fn) and not force and os.path.getsize(fn) > 500:
        return open(fn, encoding="iso-8859-1", errors="replace").read()
    raw = _curl(url)
    # one retry if it looks like the nav shell (Javascript notice, no data)
    if b"Wahlberechtigte" not in raw and b"<option value='" not in raw:
        time.sleep(DELAY)
        raw = _curl(url)
    open(fn, "wb").write(raw)
    time.sleep(DELAY)
    return raw.decode("iso-8859-1", "replace")


def num(s):
    s = re.sub(r"[^\d]", "", s or "")
    return int(s) if s else None


def pct(s):
    s = (s or "").replace(" ", "").replace(",", ".")
    try:
        return round(float(s) / 100, 4)
    except ValueError:
        return None


NUMRE = r"(\d{1,3}(?: \d{3})*|\d+)"


def _best_votes(tokens, target):
    """Split number-tokens into (votes, n_leading_name_tokens).

    A Wahlvorschlag may end in a number ("BI Ammern 90") that abuts the vote
    count, and votes may be space-grouped thousands ("1 411"). Enumerate every
    suffix that is a valid thousands-formatted number and pick the one closest
    to `target` (= share*valid). Falls back to the full number when no target.
    """
    valid_suffixes = []  # (k, value)
    for k in range(len(tokens)):
        if not re.fullmatch(r"\d{1,3}", tokens[k]):
            continue
        if all(re.fullmatch(r"\d{3}", t) for t in tokens[k + 1:]):
            valid_suffixes.append((k, int("".join(tokens[k:]))))
    if not valid_suffixes:
        return int("".join(tokens)), 0
    if target is None:
        return valid_suffixes[0][1], valid_suffixes[0][0]   # full number
    k, val = min(valid_suffixes, key=lambda kv: abs(kv[1] - target))
    return val, k


def parse_round(text, default_date):
    """Parse one round-block of plain text -> dict(aggregates + candidates)."""
    o = {}
    o["amt"] = ("hauptamtlich" if re.search(r"\bhauptamtlich", text)
                else ("ehrenamtlich" if "ehrenamtlich" in text else None))
    for k, lab in [("eligible", "Wahlberechtigte"), ("voters", "Wähler"),
                   ("invalid", "Ungültige Stimmen"), ("valid", "Gültige Stimmen")]:
        m = re.search(lab + r"\s+" + NUMRE + r"(?:\s|$)", text)
        o[k] = num(m.group(1)) if m else None
    cands = []
    valid = o.get("valid")
    reg = re.search(r"Wahlvorschlag\s+Stimmen\s+%\s+Grafik\s*"
                    r"(?:Wahl ohne Bindung[^0-9]*)?(.*)", text)
    if reg:
        body = reg.group(1)
        # votes may be space-grouped thousands ("2 335") OR a Wahlvorschlag ending
        # in a number ("BV 49" -> "BV 49 269"). The printed share pins the true
        # count: pick the votes interpretation closest to share*valid.
        for cm in re.finditer(r"(\d+)\s+(.+?)\s+(\d[\d ]*\d|\d)\s+(\d{1,3},\d)(?=\s|$)", body):
            wv = cm.group(2).strip()
            parts = cm.group(3).split()
            share = pct(cm.group(4))
            target = share * valid if (valid and share is not None) else None
            votes, k = _best_votes(parts, target)
            if k:                                     # leading tokens belong to the name
                wv = (wv + " " + " ".join(parts[:k])).strip()
            cands.append({"wahlvorschlag": wv, "votes": votes, "share": share})
    o["cands"] = cands
    return o


def parse_page(raw, gemnr, sel_date):
    """Return list of round-dicts for a Gemeinde election page."""
    soup = BeautifulSoup(raw, "html.parser")
    plain = re.sub(r"[ \t\xa0\r\n]+", " ", soup.get_text(" ")).strip()
    # Gemeinde name: "Gemeinde <gemnr> <Name> Wahl vom ..." (or before aggregates)
    nm = re.search(r"Gemeinde\s+" + re.escape(gemnr) + r"\s+(.+?)\s+(?:Wahl vom|Wahlberechtigte|\d+\.\s*Wahlgang|Stichwahl)", plain)
    name = re.sub(r"\s+", " ", nm.group(1)).strip() if nm else None

    # Split into Hauptwahl part and (optional) Stichwahl part.
    sw_marker = re.search(r"Stichwahl am\s+(\d{2}\.\d{2}\.\d{4})", plain)
    if sw_marker:
        hw_text = plain[:sw_marker.start()]
        sw_text = plain[sw_marker.start():]
        sw_date = sw_marker.group(1)
    else:
        hw_text, sw_text, sw_date = plain, None, None

    rounds = []
    hw = parse_round(hw_text, sel_date)
    if hw["cands"]:
        hw["round"] = "hauptwahl"
        hw["date"] = sel_date
        # winner line (won outright)
        wm = re.search(r"entfallen auf folgenden Bewerber:\s*(.+?)(?:\s+Home|\s+Von |\s+Kein |$)", hw_text)
        hw["winner"] = wm.group(1).strip() if wm else None
        rounds.append(hw)
    if sw_text:
        sw = parse_round(sw_text, sw_date)
        if sw["cands"]:
            sw["round"] = "stichwahl"
            sw["date"] = sw_date
            wm = re.search(r"(?:höchste Stimmenzahl|entfällt die höchste|entfallen auf folgenden Bewerber)[^:]*:\s*(.+?)(?:\s+Home|$)", sw_text)
            sw["winner"] = wm.group(1).strip() if wm else None
            rounds.append(sw)
    return name, rounds


def main():
    # 1) seed Gemeinden from each Landkreis's COMPLETE Gemeinde listing.
    #    The wJahr=0000 landing page is incomplete (it misses many Gemeinden,
    #    e.g. Apolda), but the per-Landkreis GEM view lists them all.
    LANDKREISE = [f"{n:03d}" for n in range(61, 78)]   # 061..077
    seed = {}                                          # gemnr -> wknr
    for wk in LANDKREISE:
        raw = fetch(f"{BASE}wahl.asp?wahlart=BM&wJahr=0000&zeigeErg=GEM&wknr={wk}", f"lk_{wk}")
        for gm in set(re.findall(r"gemnr=(\d+)", raw)):
            if gm[:2] in KFS_PREFIX:
                continue                   # kreisfreie Städte handled elsewhere
            seed[gm] = wk
    print(f"Seed Gemeinden (non-kreisfrei): {len(seed)}", flush=True)

    # 2) enumerate each Gemeinde's election dates from its dropdown
    elections = {}    # (wknr,gemnr) -> set(dates)
    for i, (gm, wk) in enumerate(sorted(seed.items()), 1):
        url = f"{BASE}wahl.asp?wahlart=BM&wJahr=0000&zeigeErg=GEM&wknr={wk}&gemnr={gm}"
        raw = fetch(url, f"enum_{gm}")
        dates = re.findall(r"<option value='(\d{2}\.\d{2}\.\d{4})'", raw)
        elections[(wk, gm)] = set(dates)
        if i % 50 == 0:
            print(f"  enumerated {i}/{len(seed)}", flush=True)
    total_dates = sum(len(d) for d in elections.values())
    print(f"Enumerated {total_dates} (gemnr,date) elections across {len(elections)} Gemeinden", flush=True)

    # 3) fetch + parse each (gemnr, date)
    rows = []
    fetched = 0
    skipped_future = skipped_noresult = 0
    for (wk, gm), dates in sorted(elections.items()):
        for dt in sorted(dates, key=lambda d: (d[6:], d[3:5], d[:2])):
            try:
                d = datetime.date(int(dt[6:]), int(dt[3:5]), int(dt[:2]))
            except ValueError:
                continue                    # placeholder/sentinel option (e.g. 00.00.0000)
            if d.year < 1990 or d > TODAY:
                skipped_future += 1
                continue
            url = f"{BASE}wahl.asp?wahlart=BM&wJahr=0000&zeigeErg=GEM&wknr={wk}&gemnr={gm}&w_datum={dt}"
            raw = fetch(url, f"{gm}_{dt.replace('.', '')}")
            fetched += 1
            # confirm this page is actually showing the requested date
            selm = re.search(r"value='(\d{2}\.\d{2}\.\d{4})'\s+selected", raw)
            if not selm or selm.group(1) != dt:
                skipped_noresult += 1
                continue
            name, rounds = parse_page(raw, gm, dt)
            ags = "16" + gm.zfill(6)
            for rd in rounds:
                a = rd
                rdt = rd.get("date") or dt          # each round its OWN date (SW != HW date)
                edate = f"{rdt[6:]}-{rdt[3:5]}-{rdt[:2]}"
                ryear = int(rdt[6:])
                turnout = (a["voters"] / a["eligible"]) if (a.get("voters") and a.get("eligible")) else None
                for c in rd["cands"]:
                    wv = c["wahlvorschlag"]
                    is_name = bool(re.search(r"[A-Za-zÄÖÜäöü]+,\s*[A-Za-zÄÖÜäöü]", wv))
                    rows.append(dict(
                        ags=ags, ags_name=name or "", state="16",
                        state_name="Thüringen", election_year=ryear,
                        election_date=edate,
                        election_type="Bürgermeisterwahl", round=rd["round"],
                        amt=rd.get("amt") or "",
                        eligible_voters=a.get("eligible"), number_voters=a.get("voters"),
                        valid_votes=a.get("valid"), invalid_votes=a.get("invalid"),
                        turnout=round(turnout, 4) if turnout else "",
                        candidate_name=wv if is_name else "",
                        candidate_party=wv,
                        candidate_votes=c["votes"],
                        candidate_voteshare=c["share"] if c["share"] is not None else "",
                        winner_name_raw=rd.get("winner") or "",
                        gemnr=gm, source="wahlen.thueringen.de",
                    ))
        if fetched and fetched % 200 == 0:
            print(f"  fetched {fetched} pages, {len(rows)} candidate rows so far", flush=True)

    cols = ["ags", "ags_name", "state", "state_name", "election_year", "election_date",
            "election_type", "round", "amt", "eligible_voters", "number_voters",
            "valid_votes", "invalid_votes", "turnout", "candidate_name",
            "candidate_party", "candidate_votes", "candidate_voteshare",
            "winner_name_raw", "gemnr", "source"]
    with open(OUT_CSV, "w", newline="", encoding="utf-8") as fh:
        w = csv.DictWriter(fh, fieldnames=cols)
        w.writeheader()
        for r in rows:
            w.writerow(r)

    n_elec = len({(r["ags"], r["election_date"]) for r in rows})
    print(f"\nDONE: {len(rows)} candidate rows | {n_elec} (Gemeinde,round-date) results | "
          f"{len({r['ags'] for r in rows})} Gemeinden", flush=True)
    print(f"  fetched={fetched} skipped_future={skipped_future} skipped_noresult={skipped_noresult}", flush=True)
    print(f"  -> {OUT_CSV}", flush=True)


if __name__ == "__main__":
    main()
