#!/usr/bin/env python3
"""Stage-0 parser for the Hessen mayoral candidate NAMES 1993-2012 in Hessami (2018).

Source: Hessami, Zohal (2018). "Accountability and Incentives of Appointed and
Elected Public Officials." Review of Economics and Statistics 100(1): 51-64.
Replication data: Harvard Dataverse, doi:10.7910/DVN/FZWOMK, licence CC0.
Files (copied verbatim, see data/mayoral_elections/raw/hessen/hessami_2018/README.md):
  * mayorelections.dta — one row per Bürgermeister/OB election ROUND 1993-2012
    in all 426 Gemeinden: up to 11 candidates ("Last, First", sometimes with an
    academic title), their Wahlvorschlagsträger and votes, the elected person
    (`gewaehlt`) and their gender (`geschl`). From the Hessisches Statistisches
    Landesamt, division "Wahlen und Bevölkerung".
  * collected.dta — the author's list of mayoral elections ("D") and council
    appointments ("I") with the elected mayor's name; runs into 2013.
Both name LOSING candidates, so they are gitignored and so is this script's
output; 00_he_hist_parse.py publishes only the elected persons' names.

Name parsing. The source writes "Last, First", with a title in front of the
surname ("Dr. Muster, Wolfgang") or, rarely, in front of the given name
("Muster, Dr. Gerhard"). 14 of 4,345 strings break that convention and are
repaired and flagged in `name_parse_flag`:
  * a missing comma — the order is decided by a given-name lexicon built from the
    source's own comma-separated strings;
  * ";" or "." as the separator;
  * a surname alone;
  * one cell holding two candidates' names — the first is kept; the second is the
    neighbouring candidate.
`candidate_name` is the canonical "Title Last, First" string. The title is kept out
of candidate_last_name / candidate_first_name (GERDA convention).
Unit tests: code/checks/test_he_hessami_names.py.

Outputs (gitignored, scientific use only):
  he_hessami_parsed.csv            one row per candidate per round
  he_hessami_collected_parsed.csv  election rows of collected.dta, parsed names

    python3 code/mayoral_elections/00_he_hessami_parse.py
"""

import csv
import os
import re
import unicodedata
from datetime import date

HERE = os.path.dirname(os.path.abspath(__file__))
ROOT = os.path.dirname(os.path.dirname(HERE))
RAW_DIR = os.path.join(ROOT, "data", "mayoral_elections", "raw", "hessen")
SRC_DIR = os.path.join(RAW_DIR, "hessami_2018")
ELECTIONS = os.path.join(SRC_DIR, "mayorelections.dta")
COLLECTED = os.path.join(SRC_DIR, "collected.dta")
OUT = os.path.join(RAW_DIR, "he_hessami_parsed.csv")
OUT_COLLECTED = os.path.join(RAW_DIR, "he_hessami_collected_parsed.csv")

SOURCE = "Hessami (2018), REStat 100(1), doi:10.7910/DVN/FZWOMK"

# Academic titles at the start of a name part; repeated titles stack.
TITLE_RE = re.compile(
    r"^((Prof\.|Dr\.(\s*(jur|med|rer\.\s*nat|phil|h\.\s*c)\.)?)\s*)+")
SUFFIX_RE = re.compile(r"\s+(sen\.|jun\.|sen|jun)$")
UMLAUT = str.maketrans({"ä": "ae", "ö": "oe", "ü": "ue", "ß": "ss"})


def squish(x):
    if x is None or (isinstance(x, float) and x != x):
        return ""
    return " ".join(str(x).replace(" ", " ").split())


def normalise_title(t):
    return re.sub(r"\.(?=\S)", ". ", squish(t))


def key_part(x):
    """Lower-case ASCII key: ä->ae, ß->ss, other accents stripped, hyphens and
    apostrophes as spaces, so 'Weiß'/'Weiss' and 'Hans-Peter'/'Hans Peter' match."""
    if not x:
        return ""
    y = x.lower().translate(UMLAUT)
    y = "".join(c for c in unicodedata.normalize("NFKD", y)
                if not unicodedata.combining(c))
    y = re.sub(r"[-'’]", " ", y)
    y = re.sub(r"[^a-z ]", "", y)
    return " ".join(y.split())


def _case_break(s):
    """Index of the first lower->upper case transition (two cells pasted
    together), or None."""
    for i in range(1, len(s)):
        if s[i - 1].islower() and s[i].isupper():
            return i
    return None


def parse_name(raw, lexicon=frozenset()):
    """Split one source string into title / last / first / suffix + a flag."""
    res = {"title": "", "last": "", "first": "", "suffix": "", "flag": "ok"}
    s = squish(raw)
    if not s:
        res["flag"] = "empty"
        return res
    flags = []
    if s.count(",") > 1 and _case_break(s) is not None:
        s = s[:_case_break(s)]
        flags.append("merged_cell_truncated")
    if ";" in s:
        s = re.sub(r"\s*;\s*", ", ", s)
        flags.append("separator_fixed")
    if "," not in s and re.match(
            r"^(?!(Dr|Prof)\.)[^\W\d_][\w-]{2,}\.\s+[^\W\d_a-z][^\W\d_]+$", s):
        s = re.sub(r"\.\s+", ", ", s, count=1)
        flags.append("separator_fixed")

    titles = []
    m = TITLE_RE.match(s)
    if m:
        titles.append(m.group(0))
        s = squish(s[m.end():])

    if "," in s:
        last, first = (squish(p) for p in s.split(",", 1))
        m2 = TITLE_RE.match(first)
        if m2:
            titles.append(m2.group(0))
            first = squish(first[m2.end():])
    else:
        toks = s.split(" ")
        if len(toks) == 1:
            last, first = toks[0], ""
            flags.append("last_name_only")
        elif len(toks) == 2:
            in1 = toks[0].split("-")[0].lower() in lexicon
            in2 = toks[1].split("-")[0].lower() in lexicon
            if in1 and not in2:
                last, first = toks[1], toks[0]
                flags.append("no_comma_first_last")
            elif in2 and not in1:
                last, first = toks[0], toks[1]
                flags.append("no_comma_last_first")
            else:   # source convention is Last First; flagged as a guess
                last, first = toks[0], toks[1]
                flags.append("no_comma_order_ambiguous")
        else:
            last, first = s, ""
            flags.append("unparsed")

    m3 = SUFFIX_RE.search(first) if first else None
    if m3:
        suf = m3.group(1)
        res["suffix"] = suf if suf.endswith(".") else suf + "."
        first = squish(first[:m3.start()])

    res["title"] = normalise_title(" ".join(titles)) if titles else ""
    res["last"], res["first"] = last, first
    if flags:
        res["flag"] = ";".join(dict.fromkeys(flags))
    return res


def canonical_name(p):
    if not p["last"]:
        return ""
    core = p["last"] + (", " + p["first"] if p["first"] else "")
    return squish(f"{p['title']} {core}")


def name_key(p):
    return f"{key_part(p['last'])}|{key_part(p['first'])}" if p["last"] else ""


def build_lexicon(strings):
    """Given names = first token of the given-name part of comma-separated strings."""
    lex = set()
    for s in strings:
        if s.count(",") != 1:
            continue
        first = squish(s.split(",", 1)[1])
        m = TITLE_RE.match(first)
        if m:
            first = squish(first[m.end():])
        tok = re.split(r"[ -]", first)[0].lower() if first else ""
        if tok:
            lex.add(tok)
    return frozenset(lex)


def mdy(x):
    mo, d, y = (int(v) for v in str(x).split("/"))
    return date(y, mo, d)


def main():
    import pandas as pd  # only needed for the .dta files

    if not os.path.exists(ELECTIONS):
        raise SystemExit(f"Hessami (2018) mayorelections.dta not found: {ELECTIONS}\n"
                         "Download it from doi:10.7910/DVN/FZWOMK (see the README).")
    df = pd.read_stata(ELECTIONS, convert_categoricals=False)
    assert len(df) == 1793, f"expected 1,793 rounds, got {len(df)}"

    strings = [squish(s) for i in range(1, 12) for s in df[f"kandidat_{i}"] if squish(s)]
    lexicon = build_lexicon(strings)

    out, n_flag = [], {}
    for _, r in df.iterrows():
        ags = "06" + f"{int(r['gkz']):06d}"
        d = mdy(r["wahltag"]).isoformat()
        rnd = "stichwahl" if squish(r["stichwahl"]) == "Stichwahl" else "hauptwahl"
        gew = squish(r["gewaehlt"])
        gew_key = name_key(parse_name(gew, lexicon)) if gew else ""
        gender = {"m": "m", "f": "w"}.get(squish(r["geschl"]), "")
        for i in range(1, 12):
            raw = squish(r[f"kandidat_{i}"])
            if not raw:
                continue
            p = parse_name(raw, lexicon)
            n_flag[p["flag"]] = n_flag.get(p["flag"], 0) + 1
            out.append({
                "ags": ags, "election_date": d, "round": rnd, "ballot_position": i,
                "candidate_votes": int(r[f"stimmen_kand{i}"]),
                "valid_votes": int(r["gueltige_stimmen"]),
                "candidate_party_source": squish(r[f"traeger_kand{i}"]),
                "candidate_name": canonical_name(p),
                "candidate_last_name": p["last"],
                "candidate_first_name": p["first"],
                "candidate_title": p["title"],
                "candidate_name_suffix": p["suffix"],
                "candidate_name_raw": raw,
                "name_parse_flag": p["flag"],
                "name_key": name_key(p),
                "source_winner_field": "TRUE" if gew_key and gew_key == name_key(p) else "FALSE",
                "source_winner_gender": gender,
                "source": SOURCE,
            })
    assert len(out) == 4345, f"expected 4,345 named candidate slots, got {len(out)}"
    assert sum(v for k, v in n_flag.items() if k != "ok") == 14, n_flag
    with open(OUT, "w", newline="", encoding="utf-8") as fh:
        w = csv.DictWriter(fh, fieldnames=list(out[0]))
        w.writeheader()
        w.writerows(out)
    print(f"Hessami (2018): {len(df)} rounds, {len(out)} candidate rows -> {OUT}")
    print(f"  name parse flags: {dict(sorted(n_flag.items()))}")

    if not os.path.exists(COLLECTED):
        print("  collected.dta not found — no 2013 winner names")
        return
    co = pd.read_stata(COLLECTED, convert_categoricals=False)
    rows = []
    for _, r in co[co["wahlregel"] == "D"].iterrows():
        p = parse_name(r["gewaehlt_new"], lexicon)
        dd = squish(r["wrong_eledate"])
        rows.append({
            "ags": "06" + f"{int(r['code']):06d}", "year": int(r["year"]),
            "source_date": (date(int(dd[6:]), int(dd[3:5]), int(dd[:2])).isoformat()
                            if re.match(r"^\d\d\.\d\d\.\d{4}$", dd) else ""),
            "candidate_name": canonical_name(p),
            "candidate_last_name": p["last"],
            "candidate_first_name": p["first"],
            "candidate_title": p["title"],
            "name_parse_flag": p["flag"],
            "name_key": name_key(p),
            "winner_gender": {1: "w", 0: "m"}.get(r["weiblich"], ""),
            "winner_party": squish(r["traeger_new"]),
            "source": SOURCE,
        })
    with open(OUT_COLLECTED, "w", newline="", encoding="utf-8") as fh:
        w = csv.DictWriter(fh, fieldnames=list(rows[0]))
        w.writeheader()
        w.writerows(rows)
    print(f"  collected.dta: {len(rows)} election rows -> {OUT_COLLECTED}")


if __name__ == "__main__":
    main()
