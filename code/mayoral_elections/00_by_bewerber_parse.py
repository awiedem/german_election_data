#!/usr/bin/env python3
"""Stage-0 parser for the Bayern candidate lists (Statistische Berichte B VII 3-2,
"Kommunalwahlen in Bayern: Wahlvorschläge, Bewerber"), 2008 / 2014 / 2020 / 2026.

Tables 3 and 4 list every candidate for Erster Bürgermeister / Oberbürgermeister in
Gemeinden with more than 10 000 inhabitants (Table 3) and for Landrat (Table 4) on the
main Kommunalwahl day. Each candidate is printed as name, profession and Wahlvorschlag.
The source workbook "Wahlen seit 1945" holds the same elections with every
Wahlvorschlag (Kennwort) and its votes, but no names. Each printed candidate is
assigned to a workbook Wahlvorschlag of the same election by label similarity, with
the counts required to agree.

Privacy (user decision 2026-10-07): the elected person's name is public, like every
other GERDA winner. Losing candidates' names go to the restricted twin only:
01b_mayoral_candidates.R sets `name_restricted` on them.

Sources: data/mayoral_elections/raw/bayern/lfstat_verzeichnisse/B7320C_<year>_Bewerber.pdf.
2014 and 2026 are read from their text layer. 2008 (broken font encoding) and 2020
(image only) are read from the OCR cache written by 00_by_verzeichnis_ocr.py.
Output (gitignored, names losing candidates):
  data/mayoral_elections/raw/bayern/by_bewerber_parsed_restricted.csv
Run:  python3 code/mayoral_elections/00_by_verzeichnis_ocr.py B7320C_2020_Bewerber B7320C_2008_Bewerber
      python3 code/mayoral_elections/00_by_bewerber_parse.py
"""

import csv
import datetime as dt
import difflib
import importlib.util
import os
import re
from collections import Counter, defaultdict

import pdfplumber

HERE = os.path.dirname(os.path.abspath(__file__))
_spec = importlib.util.spec_from_file_location("vz", os.path.join(HERE, "00_by_verzeichnis_parse.py"))
vz = importlib.util.module_from_spec(_spec)
_spec.loader.exec_module(vz)

SRC = vz.SRC
OUT = os.path.join(vz.RAW, "by_bewerber_parsed_restricted.csv")
BY2026 = vz.BY2026
REPORTS = {   # year -> (pdf stem, main election day, text source)
    2008: ("B7320C_2008_Bewerber", dt.date(2008, 3, 2), "ocr"),
    2014: ("B7320C_2014_Bewerber", dt.date(2014, 3, 16), "text"),
    2020: ("B7320C_2020_Bewerber", dt.date(2020, 3, 15), "ocr"),
    2026: ("B7320C_2026_Bewerber", dt.date(2026, 3, 8), "text"),
}

UNIT_BM = re.compile(r"^(\d{3})\s?(\d{3})\s+(.+)\s+\(([^()]*/[^()]*)\)\s*$")
UNIT_LR = re.compile(r"^(\d{3})\s+(.+?)\s+\(([^()/]*)\)\s*$")
NAME_LINE = re.compile(r"^(.+?)\s+(-|–|—|_|\d{1,3}\s?[,.]\s?\d)\s*$")
SEPARATOR = re.compile(r"^[×xX'´`\-_=~.]{8,}$")
SKIP = re.compile(r"^(Noch:|Bewerber|Beruf$|Wahlvorschlag|Gemeinde|Landkreis\b|Vorwahl|Bisheriger|"
                  r"Stimmen in|Schl\.|Name$|soweit dieser|anderen Wahlvorschlag|\d{4}\s+nicht oder|Statistische Berichte|"
                  r"Bayerisches Landesamt|Kommunalwahlen in Bayern|Wahlvorschläge, Bewerber|Statistik$|"
                  r"!|Ô|\d+\.\s+Bewerber)")


def text_lines(stem, mode):
    """(page, line) pairs in reading order."""
    out = []
    if mode == "text":
        with pdfplumber.open(os.path.join(SRC, stem + ".pdf")) as pdf:
            for i, pg in enumerate(pdf.pages, 1):
                for l in (pg.extract_text() or "").splitlines():
                    out.append((i, l.strip()))
    else:
        # The register loader drops the left margin and lone dashes; here the Schl.-Nr.
        # sits at the margin and a dash is the "no previous share" marker, so read the
        # OCR word table unfiltered.
        d = os.path.join(vz.CACHE, stem)
        for f in sorted(os.listdir(d)):
            p = int(f[1:5])
            groups = defaultdict(list)
            with open(os.path.join(d, f), encoding="utf-8") as fh:
                for r in csv.DictReader(fh, delimiter="\t", quoting=csv.QUOTE_NONE):
                    if r["level"] == "5" and (r["text"] or "").strip():
                        groups[(r["block_num"], r["par_num"], r["line_num"])].append(
                            (int(r["left"]), int(r["top"]), r["text"].strip()))
            lines = sorted((min(w[1] for w in ws), " ".join(w[2] for w in sorted(ws)))
                           for ws in groups.values())
            for _, txt in lines:
                txt = re.sub(r"\s+[_—–]+$", " -", txt)
                out.append((p, txt.strip()))
    return out


def section_of(line):
    if re.search(r"Bewerber zur Wahl der ersten B", line):
        return "BM"
    if re.search(r"Bewerber zur Wahl der Landr", line):
        return "LR"
    if re.search(r"^\d+\.?\s+(Wahlvorschläge|Häufigkeit)|Stadtratswahlen|Kreistagswahlen", line):
        return None
    return "keep"


def parse_report(year):
    stem, day, mode = REPORTS[year]
    units, cur, section, after_sep = [], None, None, False
    for page, line in text_lines(stem, mode):
        if not line:
            continue
        sec = section_of(line)
        if sec in ("BM", "LR"):
            section = sec
            continue
        if sec is None and re.match(r"^(Noch:\s*)?\d+\.?\s", line):
            section = None
            continue
        if section is None:
            continue
        m = UNIT_BM.match(line) if section == "BM" else UNIT_LR.match(line)
        if m:
            if section == "BM":
                ags = "09" + m.group(1) + m.group(2)
                name = m.group(3)
            else:
                ags = "09" + m.group(1) + "000"
                name = m.group(2)
            cur = {"year": year, "day": day, "section": section, "ags": ags, "unit": name,
                   "page": page, "blocks": []}
            units.append(cur)
            after_sep = False
            continue
        if cur is None or SKIP.match(line):
            continue
        # page-header table rules and title continuations ("╏", "ô", "in Gemeinden mit mehr als")
        if re.search(r"[╏ôÔ±!]", line) or re.match(r"^(in Gemeinden mit mehr|am \d+\. März|Landräten?\s)", line):
            continue
        if SEPARATOR.match(line.replace(" ", "")):
            after_sep = True
            continue
        nm = NAME_LINE.match(line)
        if nm and not re.search(r"\d{4,}", nm.group(1)):
            cur["blocks"].append({"name": nm.group(1).strip(), "vorwahl": nm.group(2),
                                  "lines": [], "incumbent_out": after_sep})
            continue
        if cur["blocks"]:
            cur["blocks"][-1]["lines"].append(line)
    return units


def split_name(raw):
    raw = re.sub(r"\s+", " ", raw).strip(" ,")
    # "Seidl jun., Hans" -> surname Seidl, given name "Hans jun." (as in the registers)
    suf = re.search(r"\s+(sen\.?|jun\.?)(?=,|$)", raw, re.I)
    if suf:
        raw = (raw[:suf.start()] + raw[suf.end():]).strip(" ,")
        last, first, title = split_name(raw)
        return last, (first + " " + suf.group(1)).strip(), title
    if "," in raw:
        last, first = raw.split(",", 1)
        toks = first.split()
        title = " ".join(t for t in toks if t.lower() in vz.TITLES)
        first = " ".join(t for t in toks if t.lower() not in vz.TITLES)
        return last.strip(), first.strip(), title
    return vz.parse_name(raw.split(), given_first=False)


PARTY_LONG = [
    (r"sozialdemokratische partei deutschlands", "spd"),
    (r"christlich[- ]soziale union( in bayern)?( e\.?v\.?)?", "csu"),
    (r"bündnis 90\s*/\s*die grünen", "grüne"),
    (r"freie demokratische partei", "fdp"),
    (r"ökologisch[- ]demokratische partei", "ödp"),
    (r"alternative für deutschland", "afd"),
    (r"bayernpartei", "bp"),
    (r"die linke", "linke"),
]
PSEUDO_WV = re.compile(r"ohne Wahlvorschlag|weitere Person|andere Person|sonstige Person", re.I)


def norm_label(s):
    s = (s or "").lower()
    for rx, short in PARTY_LONG:
        s = re.sub(rx, short, s)
    return vz.norm(s)


def label_sim(a, b):
    a2, b2 = norm_label(a), norm_label(b)
    a, b = (a or "").lower(), (b or "").lower()
    for rx, short in PARTY_LONG:
        a, b = re.sub(rx, short, a), re.sub(rx, short, b)
    if not a2 or not b2:
        return 0.0
    if a2 == b2:
        return 1.0
    r = difflib.SequenceMatcher(None, a2, b2).ratio()
    if a2 in b2 or b2 in a2:
        r = max(r, 0.85)
    ta = set(re.findall(r"[a-zäöüß]{3,}", (a or "").lower()))
    tb = set(re.findall(r"[a-zäöüß]{3,}", (b or "").lower()))
    if ta and tb:
        r = max(r, len(ta & tb) / len(ta | tb))
    return r


def workbook_candidates():
    """(ags, HW date) -> list of Wahlvorschläge with votes, from the raw workbook and 2026 file."""
    import openpyxl
    wb = openpyxl.load_workbook(vz.WORKBOOK, read_only=True)
    ws = wb["20251114_bewerberRBZ1-7"]
    out = defaultdict(list)
    for i, r in enumerate(ws.iter_rows(values_only=True)):
        if i == 0 or r[0] is None or r[2] is None:
            continue
        if (r[3] or "") not in ("erster Wahlgang", ""):
            continue
        key = ("09" + str(int(r[0])).zfill(6), r[2].date())
        out[key].append({"kennwort": (r[11] or "").strip(), "votes": r[12], "winner_wv": True})
        for j in range(14, 40, 2):
            if r[j]:
                out[key].append({"kennwort": str(r[j]).strip(), "votes": r[j + 1], "winner_wv": False})
    if os.path.exists(BY2026):
        with open(BY2026, encoding="utf-8") as fh:
            for r in csv.DictReader(fh):
                if r["round"] != "hauptwahl":
                    continue
                key = (r["ags"], dt.date.fromisoformat(r["election_date"]))
                out[key].append({"kennwort": r["candidate_party"].strip(), "votes": r["candidate_votes"],
                                 "winner_wv": None})
    return out


def assign(blocks, wvs):
    """Greedy best-first assignment of printed candidates to workbook Wahlvorschläge."""
    pairs = []
    for i, b in enumerate(blocks):
        for j, w in enumerate(wvs):
            pairs.append((label_sim(b["wv"], w["kennwort"]), i, j))
    pairs.sort(reverse=True)
    used_b, used_w, res = set(), set(), {}
    for s, i, j in pairs:
        if i in used_b or j in used_w:
            continue
        res[i] = (j, s)
        used_b.add(i); used_w.add(j)
    return res


def main():
    wbc = workbook_candidates()
    rows, stats = [], Counter()
    for year in REPORTS:
        units = parse_report(year)
        for u in units:
            blocks = [b for b in u["blocks"] if not b["incumbent_out"]]
            for b in blocks:
                ls = b["lines"]
                b["beruf"] = ls[0] if len(ls) >= 2 else ""
                b["wv"] = " ".join(ls[1:]) if len(ls) >= 2 else (ls[0] if ls else "")
                # page-header words bleed into the last block on a page in the OCR'd years
                b["wv"] = re.sub(r"\s*(Beruf,?|N?ame,?|Name,?)(\s+(Beruf,?|N?ame,?))*\s*$", "", b["wv"]).strip()
            # write-in votes ("weitere Personen ohne Wahlvorschlag") are not candidates
            wvs = [w for w in wbc.get((u["ags"], u["day"]), []) if not PSEUDO_WV.search(w["kennwort"])]
            # An outgoing incumbent who does not stand again is printed last, with
            # their previous share and no profession (2026 has no separator rule).
            while len(blocks) > len(wvs) and blocks and (
                    len(blocks[-1]["lines"]) <= 1 or blocks[-1]["vorwahl"] not in ("-", "–", "—")):
                blocks = blocks[:-1]
            if not wvs:
                stats[(year, "no_workbook_election")] += 1
                continue
            if len(blocks) != len(wvs):
                stats[(year, "count_mismatch")] += 1
                continue
            res = assign(blocks, wvs)
            # Greedy pairing takes the strongest label matches first, so at most one
            # weak pair (the one left over by elimination) is acceptable.
            weak = [s for (_, s) in res.values() if s < 0.35]
            if len(res) != len(blocks) or len(weak) > 1:
                stats[(year, "weak_labels")] += 1
                continue
            keys = [(w["kennwort"], str(w["votes"])) for w in wvs]
            if len(set(keys)) != len(keys):
                stats[(year, "duplicate_wahlvorschlag")] += 1
                continue
            stats[(year, "units_ok")] += 1
            # the workbook winner: most votes among the Wahlvorschläge
            votes = [float(w["votes"]) if str(w["votes"]).replace(".", "", 1).isdigit() else -1 for w in wvs]
            top = max(range(len(wvs)), key=lambda j: votes[j])
            for i, b in enumerate(blocks):
                j, s = res[i]
                last, first, title = split_name(b["name"])
                rows.append({
                    "year": year, "ags": u["ags"], "election_date": u["day"].isoformat(),
                    "office": u["section"], "unit": u["unit"], "kennwort": wvs[j]["kennwort"],
                    "kennwort_votes": int(float(wvs[j]["votes"])) if votes[j] >= 0 else "",
                    "wahlvorschlag_printed": b["wv"], "label_sim": round(s, 3),
                    "candidate_name": f"{last}, {first}".strip(", "), "candidate_last_name": last,
                    "candidate_first_name": first, "candidate_title": title,
                    "candidate_profession": b["beruf"], "vorwahl": b["vorwahl"],
                    "hw_top_votes": j == top, "page": u["page"],
                })
    for k in sorted(stats):
        print("  ", k, stats[k])
    print(f"candidate rows: {len(rows)}")
    cols = ["year", "ags", "election_date", "office", "unit", "kennwort", "kennwort_votes", "wahlvorschlag_printed",
            "label_sim", "candidate_name", "candidate_last_name", "candidate_first_name",
            "candidate_title", "candidate_profession", "vorwahl", "hw_top_votes", "page"]
    with open(OUT, "w", newline="", encoding="utf-8") as fh:
        w = csv.DictWriter(fh, fieldnames=cols)
        w.writeheader()
        w.writerows(rows)


if __name__ == "__main__":
    main()
