#!/usr/bin/env python3
"""
Stage 0 (fetch) — the Landeswahlleiter Brandenburg's old mayoral result pages
from the Internet Archive.

From 2008 to 2017 the Landeswahlleiter published each Gemeinde's latest
Bürgermeisterwahl at wahlen.brandenburg.de/bmwahlen/<AGS10>/h (Hauptwahl) and
.../s (Stichwahl). The pages are gone from the live site; the Internet Archive
kept captures of them.

    1. The capture list: the CDX API, saved as cdx_bmwahlen.json (tracked):
       https://web.archive.org/cdx/search/cdx?url=wahlen.brandenburg.de/bmwahlen/
           &matchType=prefix&output=json&fl=timestamp,original,statuscode,digest
    2. Every capture up to 2017 of a .../bmwahlen/<AGS10>/<h|s> page, fetched raw
       ("id_" mode, the page as served, without the archive's toolbar), into
       pages/<AGS10>_<h|s>_<capture>.html (gitignored: the pages name every
       Bewerber), and listed with its sha256 in _capture_manifest.csv (tracked).

Resumable: a page already on disk is not fetched again. Polite: one request at
a time, a 2 s pause, exponential back-off on errors, 429 and 503.

Usage:  python3 code/mayoral_elections/00_bb_lwl_archive_fetch.py [--root DIR] [--refresh-cdx]
Then:   python3 code/mayoral_elections/00_bb_lwl_archive_parse.py
"""

import argparse
import csv
import hashlib
import json
import os
import re
import subprocess
import sys
import time

HERE = os.path.dirname(os.path.abspath(__file__))
ap = argparse.ArgumentParser()
ap.add_argument("--root", default=os.path.abspath(os.path.join(HERE, "..", "..")))
ap.add_argument("--refresh-cdx", action="store_true", help="query the CDX API again")
ARGS = ap.parse_args()

ARC_DIR = os.path.join(ARGS.root, "data", "mayoral_elections", "raw", "brandenburg", "lwl_bmwahlen_archive")
PAGES = os.path.join(ARC_DIR, "pages")
CDX = os.path.join(ARC_DIR, "cdx_bmwahlen.json")
MANIFEST = os.path.join(ARC_DIR, "_capture_manifest.csv")
CDX_URL = ("https://web.archive.org/cdx/search/cdx?url=wahlen.brandenburg.de/bmwahlen/"
           "&matchType=prefix&output=json&fl=timestamp,original,statuscode,digest")


def curl(url, out):
    """Fetch url to out; True on HTTP 200. Back-off on any failure."""
    for i in range(5):
        # no -L: a redirect to another capture must not be saved under this one's timestamp
        p = subprocess.run(["curl", "-sS", "-m", "60", "-w", "%{http_code}", "-o", out, url],
                           capture_output=True, text=True)
        code = p.stdout.strip()[-3:]
        if p.returncode == 0 and code == "200" and os.path.getsize(out) > 500:
            time.sleep(2)
            return True
        time.sleep(10 * 2 ** i)
    return False


def main():
    os.makedirs(PAGES, exist_ok=True)
    if ARGS.refresh_cdx or not os.path.exists(CDX):
        if not curl(CDX_URL, CDX):
            sys.exit("FATAL: the CDX query failed")
    rows = json.load(open(CDX, encoding="utf-8"))[1:]
    sel = sorted({(ts, m.groups()) for ts, url, _st, _dg in rows
                  for m in [re.search(r"bmwahlen/(\d{10})/([hs])$", url.split("?")[0])] if m and ts[:4] <= "2017"})
    failed = 0
    for n, (ts, (ags10, page)) in enumerate(sel):
        out = os.path.join(PAGES, f"{ags10}_{page}_{ts}.html")
        if os.path.exists(out) and os.path.getsize(out) > 500:
            continue
        url = f"https://web.archive.org/web/{ts}id_/http://www.wahlen.brandenburg.de/bmwahlen/{ags10}/{page}"
        if not curl(url, out):
            failed += 1
            if os.path.exists(out):
                os.remove(out)
        if n % 50 == 0:
            print(n, "of", len(sel), flush=True)
    man = []
    for f in sorted(os.listdir(PAGES)):
        m = re.fullmatch(r"(\d{10})_([hs])_(\d{14})\.html", f)
        if not m:
            continue
        b = open(os.path.join(PAGES, f), "rb").read()
        man.append(dict(file=f, ags10=m.group(1), page=m.group(2), capture=m.group(3),
                        url=f"https://web.archive.org/web/{m.group(3)}id_/http://www.wahlen.brandenburg.de/bmwahlen/{m.group(1)}/{m.group(2)}",
                        bytes=len(b), sha256=hashlib.sha256(b).hexdigest()))
    with open(MANIFEST, "w", newline="", encoding="utf-8") as fh:
        w = csv.DictWriter(fh, fieldnames=list(man[0]))
        w.writeheader()
        w.writerows(man)
    print(f"{len(sel)} captures listed, {len(man)} on disk, {failed} failed -> {MANIFEST}")


if __name__ == "__main__":
    main()
