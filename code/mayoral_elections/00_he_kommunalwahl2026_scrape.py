#!/usr/bin/env python3
"""Stage-0 scraper for the Hessen direct Bürgermeister-/OB-wahlen of 2026
(main Kommunalwahl day 15 March 2026, with Stichwahlen on 29 March and 12 April,
plus stand-alone by-elections in January and June 2026).

The Hessisches Statistisches Landesamt has not yet published these in its
"B VII m Direktwahlen" report (latest issue: May 2024, parsed by 00_he_parse.py);
when it does, that parser ingests them with full vote counts and this interim
source can be dropped. There is no official votes-level download yet, so this
scraper uses the hessenschau result pages, which give candidate name + party +
**vote share (%)** + turnout but NO absolute vote counts. The data is therefore
PERCENTAGE-ONLY (like Rheinland-Pfalz): valid_votes / candidate_votes are NA.

Design (robust against the news site's quirks):
  * Pages are ENUMERATED dynamically from two hessenschau index pages, filtered
    to result pages carrying a 2026 date in the slug (DDMMYY with YY=26, or
    DDMMYYYY with YYYY=2026). Each Gemeinde may have a Hauptwahl page and a
    Stichwahl page (on 29.03 OR 12.04 — the date varies by Gemeinde).
  * The index is INCOMPLETE (some Stichwahl pages are reachable but unlisted),
    so SUPPLEMENTAL holds known unlisted Stichwahl pages, and an INTEGRITY CHECK
    raises if any decisive-Hauptwahl winner polled < 50 % (that signals a runoff
    we are missing) — guaranteeing we never ship a wrong "winner".
  * Gemeinde -> AGS comes from a VERIFIED slug->AGS map (hard-fail on an unknown
    slug) rather than fuzzy name matching, which is unsafe for collisions
    (Homberg Efze vs Ohm, two Münster, Lauterbach, ...).
  * Winner = top vote share in the DECISIVE round (the chronologically last one;
    Stichwahl if held, else Hauptwahl).

Output (candidate-level long, %-only; HE/01b-compatible):
  data/mayoral_elections/raw/hessen/he2026_parsed.csv
Run:  python3 code/mayoral_elections/00_he_kommunalwahl2026_scrape.py
"""

import csv
import os
import re
import sys
import time

import requests

HERE = os.path.dirname(os.path.abspath(__file__))
ROOT = os.path.dirname(os.path.dirname(HERE))
RAW_DIR = os.path.join(ROOT, "data", "mayoral_elections", "raw", "hessen")
OUT = os.path.join(RAW_DIR, "he2026_parsed.csv")
HTML_DIR = os.path.join(RAW_DIR, "html_cache_2026")

BASE = "https://www.hessenschau.de"
STATE, STATE_NAME = "06", "Hessen"
HEADERS = {"User-Agent": "Mozilla/5.0 (research; GERDA mayoral data collection)"}

INDEX_URLS = [
    "/politik/wahlen/direktwahlen/index.html",
    "/politik/kommunalwahlen/kommunalwahl-2026-in-hessen-hier-alle-ergebnisse-"
    "der-23-buegermeisterwahlen-v4,kom26-buergermeisterwahlen-100.html",
]

# Stichwahl pages reachable on the site but NOT linked from the indexes.
# (url, gemeinde_slug, round, iso_date). The integrity check below guarantees
# this list is complete: a missing runoff would surface as a < 50 % "winner".
SUPPLEMENTAL = [
    ("/politik/wahlen/direktwahlen/ergebnisse-buergermeister-stichwahl-schwalbach-"
     "am-taunus-120426-v3,stichwahl_schwalbach_am_taunus-100.html",
     "schwalbach-am-taunus", "stichwahl", "2026-03-29"),  # printed date overrides slug
]

# Verified gemeinde-slug -> 8-digit AGS (state 06). Hard-fail on an unknown slug.
SLUG2AGS = {
    "langen": "06438006", "obertshausen": "06438010", "rodenbach": "06435023",
    "battenberg-eder": "06635004", "breitenbach-am-herzberg": "06632004",
    "brensbach": "06437003", "cornberg": "06632005", "edermuende": "06634002",
    "einhausen": "06431006", "eschwege": "06636003", "espenau": "06633007",
    "gross-bieberau": "06432009", "haiger": "06532011", "haunetal": "06632008",
    "hombergefze": "06634009", "langenselbold": "06435017", "lauterbach": "06535011",
    "linsengericht": "06435018", "muenster": "06432015", "neukirchen": "06634017",
    "niddatal": "06440017", "schwalbach-am-taunus": "06436011", "sontra": "06636011",
    "waldbrunn": "06533016", "waldkappel": "06636012", "wildeck": "06632020",
    "reinhardshagen": "06633022", "hanau": "06435014",
}
# canonical display names (B VII m report style) for the ags_name column
AGS_NAMES = {
    "06438006": "Langen (Hessen)", "06438010": "Obertshausen", "06435023": "Rodenbach",
    "06635004": "Battenberg (Eder)", "06632004": "Breitenbach a. Herzberg",
    "06437003": "Brensbach", "06632005": "Cornberg", "06634002": "Edermünde",
    "06431006": "Einhausen", "06636003": "Eschwege", "06633007": "Espenau",
    "06432009": "Groß-Bieberau", "06532011": "Haiger", "06632008": "Haunetal",
    "06634009": "Homberg (Efze)", "06435017": "Langenselbold",
    "06535011": "Lauterbach (Hessen)", "06435018": "Linsengericht",
    "06432015": "Münster (Hessen)", "06634017": "Neukirchen (Knüll)",
    "06440017": "Niddatal", "06436011": "Schwalbach am Taunus", "06636011": "Sontra",
    "06533016": "Waldbrunn (Westerwald)", "06636012": "Waldkappel", "06632020": "Wildeck",
    "06633022": "Reinhardshagen", "06435014": "Hanau",
}
OB_AGS = {"06435014"}  # Hanau is the only Oberbürgermeister direct election here

CAND_RE = re.compile(
    r"([A-ZÄÖÜ][A-Za-zÄÖÜäöüß.\-]+(?:\s[A-ZÄÖÜ][A-Za-zÄÖÜäöüß.\-]+){0,3})"
    r"\s*\(([A-Za-zÄÖÜäöü0-9./ +\-]{1,40}?)\):\s*([0-9]+,[0-9]+)\s*%")
TURNOUT_RE = re.compile(r"Wahlbeteiligung[^0-9]{0,30}([0-9]+,[0-9]+)\s*%")
HREF_RE = re.compile(r'href="((?:https?://www\.hessenschau\.de)?/politik/[^"]*?\.html)"')
# slug + date from a result-page href, e.g. ...buergermeisterwahl-<slug>-150326-v1,...
SLUGDATE_RE = re.compile(
    r"(?:ober)?buergermeister(?:wahl|-stichwahl)-(.+?)-(\d{6}|\d{8})-v\d")


def fetch(url):
    cache = re.sub(r"[^A-Za-z0-9]", "_", url)[-90:] + ".html"
    path = os.path.join(HTML_DIR, cache)
    if os.path.exists(path) and os.path.getsize(path) > 2000:
        return open(path, encoding="utf-8").read()
    r = requests.get(BASE + url, headers=HEADERS, timeout=30)
    if r.status_code != 200:
        return None
    os.makedirs(HTML_DIR, exist_ok=True)
    open(path, "w", encoding="utf-8").write(r.text)
    time.sleep(0.2)
    return r.text


def strip_html(t):
    import html as _h
    return re.sub(r"\s+", " ", _h.unescape(re.sub("<[^>]+>", " ", t)))


MONTHS = {"januar": "01", "februar": "02", "märz": "03", "april": "04", "mai": "05",
          "juni": "06", "juli": "07", "august": "08", "september": "09",
          "oktober": "10", "november": "11", "dezember": "12"}
DATE_RE = re.compile(r"(?:Stichwahl|Wahl|Bürgermeisterwahl|Hauptwahl)\s*am\s*"
                     r"(\d{1,2})\.\s*([A-Za-zäöü]+)\s*(\d{4})")


def extract_date(txt, want_round):
    """Read the round's date from the page prose ('… am 29. März 2026'). The URL
    slug's date token is unreliable (the site aliases pages across date strings),
    so the printed date is authoritative. Returns ISO or None."""
    for m in DATE_RE.finditer(txt):
        label = m.group(0).lower()
        is_sw = "stichwahl" in label
        if (want_round == "stichwahl") != is_sw:
            continue
        mon = MONTHS.get(m.group(2).lower())
        if mon:
            return f"{m.group(3)}-{mon}-{int(m.group(1)):02d}"
    return None


def parse_candidates(txt):
    """Return [(name, party, share_fraction)] deduped by (surname, party)."""
    best = {}                       # (surname, party) -> (nwords, name, party, share)
    for name, party, pct in CAND_RE.findall(txt):
        name = re.sub(r"\s+", " ", name).strip()
        party = re.sub(r"\s+", " ", party).strip()
        key = (name.split()[-1].lower(), party.lower())
        nwords = len(name.split())
        # keep the cleanest (fewest words) full name per candidate; the greedy
        # regex sometimes concatenates two adjacent candidates -> more words.
        if key not in best or nwords < best[key][0]:
            best[key] = (nwords, name, party, float(pct.replace(",", ".")) / 100.0)
    return [(v[1], v[2], v[3]) for v in best.values()]


def iso_from_token(d):
    if len(d) == 6:                 # DDMMYY
        return f"20{d[4:6]}-{d[2:4]}-{d[0:2]}"
    return f"{d[4:8]}-{d[2:4]}-{d[0:2]}"  # DDMMYYYY


def enumerate_pages():
    """(url, slug, round, iso_date) for every 2026 result page, index + supplemental."""
    pages, seen = [], set()
    for idx in INDEX_URLS:
        html = fetch(idx)
        if not html:
            continue
        for href in HREF_RE.findall(html):
            href = re.sub(r"^https?://www\.hessenschau\.de", "", href)  # -> path
            m = SLUGDATE_RE.search(href)
            if not m:
                continue
            slug, dtok = m.group(1), m.group(2)
            if iso_from_token(dtok)[:4] != "2026":
                continue
            if href in seen:
                continue
            seen.add(href)
            rnd = "stichwahl" if "stichwahl" in href.lower() else "hauptwahl"
            pages.append((href, slug, rnd, iso_from_token(dtok)))
    for url, slug, rnd, date in SUPPLEMENTAL:
        if url not in seen:
            seen.add(url)
            pages.append((url, slug, rnd, date))
    return pages


def split_name(full):
    parts = re.sub(r"\s+", " ", full).strip().split()
    return " ".join(parts), (parts[-1] if parts else full), " ".join(parts[:-1])


FIELDS = ["ags", "ags_name", "state", "state_name", "election_year", "election_date",
          "election_type", "round", "eligible_voters", "number_voters", "valid_votes",
          "invalid_votes", "turnout", "candidate_name", "candidate_last_name",
          "candidate_first_name", "candidate_party", "candidate_votes",
          "candidate_voteshare", "is_winner", "candidate_rank", "n_candidates",
          "source_url"]


def main():
    pages = enumerate_pages()
    rounds = {}          # ags -> {round: {date, cands, turnout, slug}}
    failed, unknown = [], set()
    for url, slug, rnd, date in pages:
        if slug not in SLUG2AGS:
            unknown.add(slug)
            continue
        html = fetch(url)
        if html is None:
            failed.append((slug, rnd, "fetch"))
            continue
        txt = strip_html(html)
        cands = parse_candidates(txt)
        if not cands:
            failed.append((slug, rnd, "no candidates"))   # e.g. Hanau HW news article
            continue
        tt = TURNOUT_RE.search(txt)
        ags = SLUG2AGS[slug]
        # the printed date ('… am 29. März 2026') is authoritative; the URL slug's
        # date token is unreliable (the site aliases a page across date strings).
        date = extract_date(txt, rnd) or date
        # a later page for the same (ags, round) supersedes (re-runs/corrections)
        prev = rounds.get(ags, {}).get(rnd)
        if prev and prev["date"] > date:
            continue
        rounds.setdefault(ags, {})[rnd] = {
            "date": date, "cands": cands, "slug": slug,
            "turnout": float(tt.group(1).replace(",", ".")) / 100.0 if tt else None}

    if unknown:
        raise SystemExit(f"FATAL: unknown gemeinde slug(s), add to SLUG2AGS: {sorted(unknown)}")

    rows, integrity = [], []
    for ags, rdict in rounds.items():
        etype = "Oberbürgermeisterwahl" if ags in OB_AGS else "Bürgermeisterwahl"
        decisive = max(rdict, key=lambda r: rdict[r]["date"])   # latest-dated round
        for rnd, rec in rdict.items():
            ranked = sorted(rec["cands"], key=lambda c: -c[2])
            for i, (name, party, share) in enumerate(ranked):
                is_winner = (rnd == decisive) and (i == 0)
                if is_winner and decisive == "hauptwahl" and share < 0.5 and len(ranked) > 1:
                    integrity.append((ags, AGS_NAMES.get(ags, ags), round(share * 100, 1)))
                full, last, first = split_name(name)
                rows.append({
                    "ags": ags, "ags_name": AGS_NAMES.get(ags, ags),
                    "state": STATE, "state_name": STATE_NAME,
                    "election_year": rec["date"][:4], "election_date": rec["date"],
                    "election_type": etype, "round": rnd,
                    "eligible_voters": "", "number_voters": "",
                    "valid_votes": "", "invalid_votes": "",   # %-only source
                    "turnout": rec["turnout"] if rec["turnout"] is not None else "",
                    "candidate_name": full, "candidate_last_name": last,
                    "candidate_first_name": first, "candidate_party": party,
                    "candidate_votes": "", "candidate_voteshare": share,
                    "is_winner": "TRUE" if is_winner else "FALSE",
                    "candidate_rank": i + 1, "n_candidates": len(ranked),
                    "source_url": rec.get("url", BASE),
                })

    n_ob = sum(1 for a in rounds if a in OB_AGS)
    n_sw = sum(1 for r in rounds.values() if "stichwahl" in r)
    sys.stderr.write(
        f"\n=== Hessen Kommunalwahl 2026 scrape (%-only) ===\n"
        f"  result pages enumerated:  {len(pages)}\n"
        f"  elections (distinct AGS): {len(rounds)}  (OB: {n_ob})\n"
        f"  with Stichwahl:           {n_sw}\n"
        f"  candidate rows:           {len(rows)}\n")
    benign = [f for f in failed if not (f[0] == "hanau" and f[1] == "hauptwahl")]
    if failed:
        sys.stderr.write(f"  pages without parse: {failed}\n")
    if integrity:
        raise SystemExit(
            "FATAL integrity: decisive Hauptwahl winner < 50% -> missing Stichwahl "
            f"page for {integrity}. Add it to SUPPLEMENTAL.")
    if benign:
        raise SystemExit(f"FATAL: unexpected parse failures: {benign}")

    with open(OUT, "w", newline="", encoding="utf-8") as fh:
        w = csv.DictWriter(fh, fieldnames=FIELDS)
        w.writeheader()
        w.writerows(rows)
    sys.stderr.write(f"  Wrote {len(rows)} rows -> {OUT}\n")


if __name__ == "__main__":
    main()
