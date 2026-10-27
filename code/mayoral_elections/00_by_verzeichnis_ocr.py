#!/usr/bin/env python3
"""OCR cache for the Bayern Bürgermeisterverzeichnisse and LfStat reports.

The scanned registers in data/mayoral_elections/raw/bayern/lfstat_verzeichnisse/
carry an embedded text layer, but it is unusable for 1984 and error-prone
elsewhere (see that folder's README). This script renders every page at 300 DPI
(greyscale) and runs Tesseract (deu, --psm 6). It caches word-level TSV per page
under _ocr_cache/<pdf-stem>/p<NNNN>.tsv. The cache is gitignored and the step is
idempotent: pages already cached are skipped.

Run:  python3 code/mayoral_elections/00_by_verzeichnis_ocr.py [pdf-stem ...]
Then: python3 code/mayoral_elections/00_by_verzeichnis_parse.py
"""

import os
import subprocess
import sys
import tempfile
from concurrent.futures import ThreadPoolExecutor

import pdfplumber

HERE = os.path.dirname(os.path.abspath(__file__))
ROOT = os.path.dirname(os.path.dirname(HERE))
SRC = os.path.join(ROOT, "data", "mayoral_elections", "raw", "bayern", "lfstat_verzeichnisse")
CACHE = os.path.join(SRC, "_ocr_cache")

DEFAULT = [
    "BY_Verzeichnis_1978_Heft375",
    "BY_Verzeichnis_1984_Heft406",
    "BY_Verzeichnis_1990_Heft463",
    "BY_Verzeichnis_1996_Heft502",
    "BY_Verzeichnis_2002_B73516",
    "BY_Verzeichnis_2008",
    "BY_Verzeichnis_2014_B73516",
    "B7370C_2008_OB_Landraete",
    "B7370C_2014_OB_Landraete",
]
DPI = "300"


def ocr_page(pdf, stem, page):
    out = os.path.join(CACHE, stem, f"p{page:04d}.tsv")
    if os.path.exists(out) and os.path.getsize(out) > 0:
        return "cached"
    with tempfile.TemporaryDirectory() as tmp:
        base = os.path.join(tmp, "pg")
        subprocess.run(["pdftoppm", "-r", DPI, "-gray", "-f", str(page), "-l", str(page),
                        "-singlefile", pdf, base], check=True)
        img = base + ".pgm"
        res = subprocess.run(["tesseract", img, "stdout", "-l", "deu", "--psm", "6", "tsv"],
                             check=True, capture_output=True, text=True)
    tmp_out = out + ".part"
    with open(tmp_out, "w", encoding="utf-8") as fh:
        fh.write(res.stdout)
    os.replace(tmp_out, out)
    return "done"


def main():
    stems = sys.argv[1:] or DEFAULT
    jobs = []
    for stem in stems:
        pdf = os.path.join(SRC, stem + ".pdf")
        os.makedirs(os.path.join(CACHE, stem), exist_ok=True)
        with pdfplumber.open(pdf) as p:
            n = len(p.pages)
        jobs += [(pdf, stem, i) for i in range(1, n + 1)]
    print(f"{len(jobs)} pages in {len(stems)} files", flush=True)
    done = 0
    with ThreadPoolExecutor(max_workers=max(1, (os.cpu_count() or 4) - 2)) as ex:
        for i, status in enumerate(ex.map(lambda j: ocr_page(*j), jobs), 1):
            done += status == "done"
            if i % 100 == 0:
                print(f"  {i}/{len(jobs)} ({done} new)", flush=True)
    print(f"finished: {done} pages OCR'd, {len(jobs) - done} cached", flush=True)


if __name__ == "__main__":
    main()
