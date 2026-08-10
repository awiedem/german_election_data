"""Build gender lookup table for mayoral candidates using gender-guesser.

Uses Jörg Michael's comprehensive name database (nam_dict.txt, ~70k names
with country-specific gender codes) via the Python `gender-guesser` package.

Requires: pip install gender-guesser

Input:  data/mayoral_elections/final/mayoral_candidates.rds (via CSV extract)
Output: data/mayoral_elections/processed/gender_guesser_lookup.csv

The lookup maps candidate_first_name → gender (m/w/empty) with method info.
The companion R script 04_candidate_characteristics.R reads this CSV.

When the gitignored restricted twin
data/mayoral_elections/final_restricted/mayoral_candidates_restricted.rds exists,
the first names it holds and the published file does not — the Sachsen-Anhalt
losing candidates — are classified into a second, also gitignored lookup,
gender_guesser_lookup_restricted.csv. The committed lookup is byte-identical
either way.
"""

import csv
import os
import re
import subprocess
import sys
import unicodedata

try:
    import gender_guesser.detector as gender
except ImportError:
    sys.exit("gender-guesser not installed. Run: pip install gender-guesser")

# ---------------------------------------------------------------------------
# Configuration
# ---------------------------------------------------------------------------

# Project root (relative to this script)
PROJ_ROOT = os.path.abspath(os.path.join(os.path.dirname(__file__), "..", ".."))
INPUT_RDS = os.path.join(PROJ_ROOT, "data", "mayoral_elections", "final", "mayoral_candidates.rds")
OUTPUT_CSV = os.path.join(PROJ_ROOT, "data", "mayoral_elections", "processed", "gender_guesser_lookup.csv")
NAMES_TMP = "/tmp/all_first_names.csv"

# Restricted (scientific-use-only) twin: same candidates, but the Sachsen-Anhalt
# losers keep their names. Its first names go to a SEPARATE, gitignored lookup
# so the committed one above stays free of them. Both files are optional inputs
# — this script is a no-op on them when the restricted twin has not been built.
RESTRICTED_RDS = os.path.join(PROJ_ROOT, "data", "mayoral_elections",
                              "final_restricted", "mayoral_candidates_restricted.rds")
RESTRICTED_OUTPUT_CSV = os.path.join(PROJ_ROOT, "data", "mayoral_elections", "processed",
                                     "gender_guesser_lookup_restricted.csv")
RESTRICTED_NAMES_TMP = "/tmp/all_first_names_restricted.csv"

# ---------------------------------------------------------------------------
# Manual gender overrides — typos, rare names, foreign names, ambiguous
# ---------------------------------------------------------------------------

MANUAL_GENDER = {
    # Typos of common names
    "Alfed": "m", "Annlies": "w", "Bertholt": "m", "Birigt": "w", "Borris": "m",
    "Chirstopher": "m", "Danielo": "m", "Detled": "m", "Ferdninand": "m",
    "Gereom": "m", "Gernhold": "m", "Gotfried": "m", "Gunar": "m", "Harmut": "m",
    "Hartmud": "m", "Hendric": "m", "Hennry": "m", "Micharel": "m", "Michale": "m",
    "Migel": "m", "Nobert": "m", "Sebastin": "m", "Stepfan": "m", "Tatjaana": "w",
    "Wolfang": "m", "Lisett": "w", "Jenz": "m", "Laars": "m", "Stev": "m",
    "Normen": "m", "Gorden": "m", "Keneth": "m", "Ulff": "m",
    # Accent variants
    "Michèl": "m", "Reneé": "m", "Renè": "m",
    # Rare German/Frisian names
    "Berko": "m", "Heico": "m", "Hejo": "m", "Heint": "m", "Lühr": "m",
    "Peik": "m", "Wilnis": "m", "Wittich": "m", "Elk": "m", "Konke": "m",
    "Irnfried": "m", "Lupart": "m", "Osmar": "m", "Wilk": "m", "Grischa": "m",
    "Rodja": "m", "Janosch": "m", "Quido": "m", "Norwich": "m", "Roul": "m",
    "Stiewen": "m", "Heider": "m", "Detert": "m", "Redelf": "m", "Meent": "m",
    "Hayo": "m", "Geritt": "m", "Nicolin": "m", "Sahver": "m", "Cort": "m",
    "Borsu": "m",
    # Turkish/Arabic/foreign
    "Oguzhan": "m", "Selcuk": "m", "Cetin": "m", "Firat": "m", "Alisan": "m",
    "Hamun": "m", "Borzoo": "m", "Birhat": "m", "Amid": "m", "Shoaiub": "m",
    "Ulas": "m", "Celil": "m", "Yildiz": "w", "Ayse": "w", "Büsranur": "w",
    "Böcek": "m", "Palitha": "m", "Pesi": "m", "Tula": "w", "Jaroslaw": "m",
    # Female
    "Alessa": "w", "Björna": "w", "Bondina": "w", "Celine": "w", "Jule": "w",
    "Klarin": "w", "Selly": "w", "Stephie": "w", "Thesa": "w", "Thoringe": "w",
    "Trudis": "w", "Verani": "w", "Karelli": "w", "Kemla": "w", "Cornelie": "w",
    "Alja": "w", "Tagrid": "w", "Uode": "w",
    # Ambiguous, resolved for German mayor context
    "Eike": "m", "Dominique": "w", "Jans": "m", "Arpad": "m", "Cajus": "m",
    # Garbled/data error
    "Fatjpn": "m", "Mtf": None, "Possienke": None,
}

# Raw first_name values that need special handling
RAW_OVERRIDES = {
    "Cort-Brün": "m",
    "Kl.-H.": None,
    "Calbitz, Malkwitz": None,
    "Putzkau, Schmölln": None,
    "Lichtenau und Ottendorf": None,
    "Lichtenau und Ottendorf e.V.": None,
    "Prof. Dr. Ing. Thomas Paul Hermann": "m",
    'Familie, Vaterland - Liste Henry Nitzsch" e. V.': None,
    'Familie, Vaterland - Liste Henry Nitzsche" e.V.': None,
    'M.E. "Marlies"': "w",
    ', Andreas': "m",
    'Dr, Hans-Christian': "m",
    'Marcel "Bratwurst"': "m",
    'Hubertus "Hubert vom Venn"': "m",
    'Georg "Schorsch"': "m",
}

# Non-person name patterns
NON_PERSON_PATTERNS = [
    r'(?i)\b(SPD|CDU|CSU|FDP|AFD)\b',
    r'(?i)GRÜNE|LINKE|BÜNDNIS',
    r'(?i)Partei|Bürger(?:bewegung)?|Liste\b|Wähler',
    r'(?i)Sozialdemokrat|Freie\s+(?:Demokrat|Wähler)',
    r'(?i)Vereinigung|Christlich|Ortsverband',
    r'(?i)\bund\b.*\b(e\.?\s*V\.|Verein)',
    r'(?i)e\.\s*V\.\s*$',
    r'(?i)Schule|Hort|Kultur\s+und\s+Sport',
    r'(?i)^OT\s',
    r'(?i)Rechtsstaat|Tierschutz|Initiative|Förderung|basisdemokrat',
    r'(?i)^sonstige$',
    r'(?i)Familie,\s*Vaterland',
    r'(?i)Lebenserfahrenen|Berufserfahrenen',
]

# ---------------------------------------------------------------------------
# Functions
# ---------------------------------------------------------------------------

def is_non_person(s):
    return any(re.search(pat, s) for pat in NON_PERSON_PATTERNS)


def clean_name(raw):
    """Extract the classifiable first name from a raw first_name field."""
    s = raw.strip()
    if not s:
        return None, "empty"

    if s in RAW_OVERRIDES:
        g = RAW_OVERRIDES[s]
        return (s, "raw_override") if g is not None else (None, "not_person")

    if s in MANUAL_GENDER:
        return s, "manual"

    if is_non_person(s):
        return None, "not_person"

    # Comma-separated: place names vs person names
    if "," in s:
        parts = [p.strip() for p in s.split(",")]
        first = parts[0].strip()
        if re.match(r'^[A-ZÄÖÜ][a-zäöüß]+$', first) and len(first) <= 15:
            s = first
        else:
            return None, "not_person"

    # Quoted names: 'Marcel "Bratwurst"' → Marcel
    quoted = re.search(r'"([^"]+)"', s)
    if quoted:
        unquoted = re.sub(r'"[^"]*"', '', s).strip()
        if re.match(r'^[A-ZÄÖÜ][a-zäöüß]+$', unquoted):
            s = unquoted
        elif re.match(r'^[A-Z]\.[A-Z]\.?$', unquoted.replace(" ", "")):
            s = quoted.group(1)
        else:
            s = unquoted if unquoted and re.match(r'^[A-ZÄÖÜ]', unquoted) else quoted.group(1)

    # Strip titles (careful not to eat names starting with "Ing")
    s = re.sub(r'^Prof\.?\s*', '', s).strip()
    s = re.sub(r'^Dr\.?\s*(Dr\.?\s*)?(jur\.?\s*|med\.?\s*|phil\.?\s*|rer\.?\s*nat\.?\s*)?', '', s).strip()
    s = re.sub(r'^-\s*Ing\.?\s*', '', s).strip()
    s = re.sub(r'^Dipl\.\s*-?\s*Ing\.?\s*', '', s).strip()
    if re.match(r'^Ing\.\s+[A-Z]', s):
        s = re.sub(r'^Ing\.\s+', '', s).strip()
    s = re.sub(r'^MdL\s+', '', s).strip()

    if s in MANUAL_GENDER:
        return s, "manual"

    s = re.sub(r'\s*\(.*?\)\s*$', '', s).strip()
    s = re.sub(r'\s+[A-Z]\.$', '', s).strip()

    # Initials
    if re.match(r'^[A-Z]\.?$', s) or re.match(r'^[A-Z]{1,2}\.$', s):
        return None, "initials"
    if re.match(r'^[A-Z]{1,3}\.-[A-Z]\.$', s):
        return None, "initials"
    if re.match(r'^Ch\.$', s):
        return None, "initials"

    if re.match(r'^[A-Z]\.\s+', s):
        s = re.sub(r'^[A-Z]\.\s+', '', s).strip()
    if re.match(r'^[A-Z]\.-', s):
        s = s.split("-", 1)[1]

    if " " in s:
        s = s.split()[0]

    if len(s) < 2:
        return None, "too_short"

    return s, "ok"


def classify_gender(name_clean, raw_name, detector):
    """Classify gender using gender-guesser + manual overrides."""
    if not name_clean:
        return None, None

    # Raw overrides
    if raw_name.strip() in RAW_OVERRIDES:
        g = RAW_OVERRIDES[raw_name.strip()]
        return (g, "manual") if g else (None, "manual_exclude")

    # Manual overrides
    for key in [raw_name.strip(), name_clean]:
        if key in MANUAL_GENDER:
            g = MANUAL_GENDER[key]
            return (g, "manual") if g is not None else (None, "manual_exclude")

    # gender-guesser
    g_de = detector.get_gender(name_clean, country="germany")
    g_global = detector.get_gender(name_clean)
    method = "full"

    # Hyphenated fallback
    if "-" in name_clean and g_de == "unknown" and g_global == "unknown":
        first_part = name_clean.split("-")[0]
        if len(first_part) >= 2:
            g_de = detector.get_gender(first_part, country="germany")
            g_global = detector.get_gender(first_part)
            method = "hyphen_first"

    # Accent-stripped fallback
    if g_de == "unknown" and g_global == "unknown":
        normalized = ''.join(c for c in unicodedata.normalize('NFD', name_clean)
                             if unicodedata.category(c) != 'Mn')
        if normalized != name_clean:
            g_de = detector.get_gender(normalized, country="germany")
            g_global = detector.get_gender(normalized)
            method = "accent_norm"

    def resolve(g):
        if g in ("male", "mostly_male"):
            return "m"
        elif g in ("female", "mostly_female"):
            return "w"
        return None

    result = resolve(g_de)
    source = "de"
    if result is None:
        result = resolve(g_global)
        source = "global"

    if result is None:
        return None, "andy" if (g_de == "andy" or g_global == "andy") else "unknown"
    return result, f"{method}_{source}"


# ---------------------------------------------------------------------------
# Main
# ---------------------------------------------------------------------------

def extract_first_names(rds_path, tmp_path):
    """Unique candidate_first_name values in an .rds candidate file, via R."""
    r_code = f'''
    library(data.table)
    cand <- readRDS("{rds_path}")
    setDT(cand)
    names_dt <- cand[!is.na(candidate_first_name), .(count = .N), by = candidate_first_name]
    fwrite(names_dt, "{tmp_path}")
    cat(sprintf("Wrote %d unique first names\\n", nrow(names_dt)))
    '''
    subprocess.run(["Rscript", "-e", r_code], capture_output=True, text=True, check=True)
    names = []
    with open(tmp_path, "r") as f:
        for row in csv.DictReader(f):
            names.append(row["candidate_first_name"])
    return names


def classify_names(names, d):
    """Run the full clean → override → gender-guesser cascade over a name list."""
    results = []
    stats = {"classified": 0, "not_person": 0, "initials": 0, "empty": 0,
             "too_short": 0, "unknown": 0, "andy": 0, "manual_exclude": 0}

    for raw_name in names:
        clean, status = clean_name(raw_name)

        if clean is None:
            results.append((raw_name, "", None, status))
            stats[status] = stats.get(status, 0) + 1
            continue

        if status in ("manual", "raw_override"):
            g = RAW_OVERRIDES.get(raw_name.strip()) or MANUAL_GENDER.get(raw_name.strip()) or MANUAL_GENDER.get(clean)
            if g is not None:
                results.append((raw_name, clean, g, "manual"))
                stats["classified"] += 1
            else:
                results.append((raw_name, clean, None, "manual_exclude"))
                stats["manual_exclude"] += 1
            continue

        gender_result, method = classify_gender(clean, raw_name, d)

        if gender_result is None:
            results.append((raw_name, clean, None, method or "unknown"))
            if method == "andy":
                stats["andy"] += 1
            elif method == "manual_exclude":
                stats["manual_exclude"] += 1
            else:
                stats["unknown"] += 1
        else:
            results.append((raw_name, clean, gender_result, method))
            stats["classified"] += 1

    return results, stats


def write_lookup(path, results):
    """Pipe-delimited so names containing double quotes ('Marcel "Bratwurst"')
    need no quoting."""
    os.makedirs(os.path.dirname(path), exist_ok=True)
    with open(path, "w") as f:
        f.write("candidate_first_name|name_clean|gender|method\n")
        for raw, clean, g, method in results:
            f.write(f"{raw}|{clean}|{g or ''}|{method}\n")


def report(results, stats, label, out_path):
    total = sum(stats.values())
    if not total:
        print(f"\n=== {label}: nothing to classify ===")
        return
    print(f"\n=== Classification Results — {label} ===")
    for k, v in sorted(stats.items(), key=lambda x: -x[1]):
        print(f"  {k}: {v} ({100*v/total:.1f}%)")

    unclassified = [(r[0], r[1], r[3]) for r in results
                    if r[2] is None and r[3] not in ("not_person", "empty", "initials", "too_short", "manual_exclude")]
    if unclassified:
        print(f"\nWARNING: {len(unclassified)} names still unclassified:")
        for raw, clean, reason in sorted(unclassified):
            print(f"  '{raw}' -> '{clean}' ({reason})")
    else:
        print(f"\nAll classifiable names have been classified.")

    print(f"\nOutput: {out_path}")
    print(f"Classified: {stats['classified']}/{total} ({100*stats['classified']/total:.1f}%)")


def main():
    d = gender.Detector()

    # ---- Published (anonymised) candidates -> the tracked lookup ----
    print("Extracting unique first names from candidates data...")
    names = extract_first_names(INPUT_RDS, NAMES_TMP)
    print(f"Total unique first_name values: {len(names)}")

    results, stats = classify_names(names, d)
    write_lookup(OUTPUT_CSV, results)
    report(results, stats, "published", OUTPUT_CSV)

    # ---- RESTRICTED twin -> a separate, gitignored lookup ----
    # The Sachsen-Anhalt losing candidates are stripped from the published file,
    # so their first names never reach the lookup above and 04 would leave them
    # genderless. Classify the EXTRA names here and keep them in their own file:
    # gender_guesser_lookup.csv is committed, and a bare list of ST loser first
    # names has no business in the repository. Scientific use only.
    if not os.path.exists(RESTRICTED_RDS):
        print("\nNo restricted candidate file — skipping the restricted lookup.")
        return

    print("\nExtracting first names from the RESTRICTED candidates data...")
    all_names = extract_first_names(RESTRICTED_RDS, RESTRICTED_NAMES_TMP)
    extra = [n for n in all_names if n not in set(names)]
    print(f"Total unique first_name values: {len(all_names)} "
          f"({len(extra)} not in the published file)")

    extra_results, extra_stats = classify_names(extra, d)
    write_lookup(RESTRICTED_OUTPUT_CSV, extra_results)
    report(extra_results, extra_stats, "restricted-only names", RESTRICTED_OUTPUT_CSV)


if __name__ == "__main__":
    main()
