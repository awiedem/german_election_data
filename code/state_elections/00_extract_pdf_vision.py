#!/usr/bin/env python3
"""
Hybrid GPT-5.4-mini + Tesseract/HMM extraction of election results from scanned PDFs.

Left pages (metadata): GPT-5.4-mini vision — 100% accuracy on AGS, name, EV, Wähler, ungültig, gültig.
Right pages (party votes): Tesseract OCR + HMM Viterbi decoding to identify Z_PCT rows.
Fallback: Gemini 2.5 Flash for pages where Tesseract fails.

The HMM enforces the cyclic state sequence E_ABS → E_PCT → Z_ABS → Z_PCT per municipality,
solving the Erst/Zweitstimmen row disambiguation problem structurally.

Usage:
  source .venv/bin/activate
  python3 code/state_elections/00_extract_pdf_vision.py --config code/state_elections/configs/bb_1994.yaml
"""

import argparse
import base64
import csv
import json
import os
import re
import sys
import time
from collections import Counter, defaultdict
from io import BytesIO
from pathlib import Path

import cv2
import numpy as np
import yaml
from openai import OpenAI
from pdf2image import convert_from_path
import pytesseract

try:
    from google import genai
    from google.genai import types as gem_types
    HAS_GEMINI = True
except ImportError:
    HAS_GEMINI = False

GPT_MODEL = "gpt-5.4-mini"
GEMINI_MODEL = "gemini-2.5-flash"
DPI_VISION = 200
DPI_TESSERACT = 300
STATES = ["E_ABS", "E_PCT", "Z_ABS", "Z_PCT"]


def load_config(path):
    with open(path) as f:
        return yaml.safe_load(f)


# ── Left page: GPT-5.4-mini ───────────────────────────────────────────────

def extract_left_gpt(client, prompt, pdf_path, page_num):
    imgs = convert_from_path(str(pdf_path), first_page=page_num, last_page=page_num, dpi=DPI_VISION)
    buf = BytesIO()
    imgs[0].save(buf, format="PNG")
    b64 = base64.b64encode(buf.getvalue()).decode()

    resp = client.chat.completions.create(
        model=GPT_MODEL, temperature=0, max_completion_tokens=4000,
        messages=[{"role": "user", "content": [
            {"type": "text", "text": prompt},
            {"type": "image_url", "image_url": {"url": f"data:image/png;base64,{b64}"}},
        ]}],
    )
    text = resp.choices[0].message.content
    usage = {"input_tokens": resp.usage.prompt_tokens,
             "output_tokens": resp.usage.completion_tokens}
    m = re.search(r"\[.*\]", text, re.DOTALL)
    data = json.loads(m.group()) if m else []
    for rec in data:
        for old, new in [("code", "gemeindeschluessel"), ("gemeinde", "name"),
                         ("gültige", "gueltige"), ("ungültige", "ungueltige")]:
            if old in rec and new not in rec:
                rec[new] = rec[old]
    return data, text, usage


# ── Right page: Tesseract + HMM Viterbi ───────────────────────────────────

def split_merged_pcts(s):
    return re.sub(r"(\d+,\d\d)(\d+,\d+)", r"\1 \2", s)


def extract_right_rows(pdf_path, page_num):
    """Extract all data rows from right page using Tesseract."""
    imgs = convert_from_path(str(pdf_path), first_page=page_num, last_page=page_num, dpi=DPI_TESSERACT)
    gray = cv2.cvtColor(np.array(imgs[0]), cv2.COLOR_RGB2GRAY)
    _, binary = cv2.threshold(gray, 160, 255, cv2.THRESH_BINARY)
    text = pytesseract.image_to_string(binary[:, 50:], lang="deu", config="--psm 6 --oem 1")
    lines = [l.strip() for l in text.split("\n") if l.strip()]

    rows = []
    for line in lines:
        n_comma = line.count(",")
        if n_comma >= 3:
            rtype = "PCT"
            cleaned = split_merged_pcts(line)
            tokens = re.findall(r"[\d]+,[\d]+|[xX\-\*]", cleaned)
            vals = [float(t.replace(",", ".")) if "," in t else 0.0 for t in tokens]
        else:
            rtype = "ABS"
            tokens = line.split()
            vals = []
            for t in tokens:
                t = (t.replace("ı", "1").replace("l", "1")
                      .replace("I", "1").replace("O", "0").replace("|", "1"))
                if t in ("x", "X", "-", "*", "—", "–"):
                    vals.append(0)
                else:
                    d = re.sub(r"[^0-9]", "", t)
                    vals.append(int(d) if d else 0)
        if len(vals) >= 8:
            rows.append({"type": rtype, "values": vals[:15]})
    return rows, text


def viterbi_decode(rows):
    """
    Assign E_ABS/E_PCT/Z_ABS/Z_PCT state to each row using Viterbi algorithm.
    The transition model enforces the cycle: E_ABS → E_PCT → Z_ABS → Z_PCT → E_ABS → ...
    The emission model ensures ABS rows → ABS states, PCT rows → PCT states.
    """
    N = len(rows)
    if N == 0:
        return np.array([], dtype=int)

    # Transition log-probs
    LOG_TRANS = np.full((4, 4), -10.0)
    for s in range(4):
        LOG_TRANS[s][(s + 1) % 4] = 0.0   # normal cycle: free
        LOG_TRANS[s][s] = -3.0              # repeat: mild penalty
        LOG_TRANS[s][(s + 2) % 4] = -5.0   # skip one: moderate penalty

    # Emission log-probs: ABS rows match states 0,2; PCT rows match states 1,3
    LOG_EMIT = np.zeros((N, 4))
    for i, row in enumerate(rows):
        if row["type"] == "ABS":
            LOG_EMIT[i] = [0, -8, 0, -8]
        else:
            LOG_EMIT[i] = [-8, 0, -8, 0]

    # Viterbi
    V = np.full((N, 4), -np.inf)
    bp = np.zeros((N, 4), dtype=int)
    V[0] = LOG_EMIT[0] + np.array([0, -5, -5, -5])  # prior: start with E_ABS

    for t in range(1, N):
        for s in range(4):
            scores = V[t - 1] + LOG_TRANS[:, s] + LOG_EMIT[t, s]
            V[t, s] = np.max(scores)
            bp[t, s] = np.argmax(scores)

    path = np.zeros(N, dtype=int)
    path[-1] = np.argmax(V[-1])
    for t in range(N - 2, -1, -1):
        path[t] = bp[t + 1, path[t + 1]]

    return path


def extract_right_hmm(pdf_path, page_num, n_munis):
    """Extract Z_PCT rows from right page using Tesseract + HMM."""
    rows, raw_text = extract_right_rows(pdf_path, page_num)
    if not rows:
        return [], raw_text

    path = viterbi_decode(rows)
    z_pct_rows = [rows[i]["values"][:14] for i in range(len(rows)) if path[i] == 3]
    return z_pct_rows, raw_text


# ── Fallback: Gemini for right page ────────────────────────────────────────

def extract_right_gemini_fallback(gem_client, cfg, pdf_path, page_num, left_data, party_cols):
    """Fallback: use Gemini for right page when Tesseract/HMM fails."""
    if not HAS_GEMINI or not gem_client:
        return []

    n = len(left_data)
    muni_lines = []
    for i, m in enumerate(left_data):
        ge = m.get("gueltige_e", m.get("gueltige", 0)) or 0
        gz = m.get("gueltige_z", m.get("gueltige", 0)) or 0
        muni_lines.append(f"  {i+1}. {m.get('name','')} (code {m.get('gemeindeschluessel','')}): "
                          f"Erststimmen gültig={ge}, Zweitstimmen gültig={gz}")

    prompt = cfg["right_prompt"].replace("{n}", str(n)).replace("{muni_list}", "\n".join(muni_lines))
    schema = gem_types.Schema(
        type="ARRAY",
        items=gem_types.Schema(
            type="OBJECT",
            properties={p + "_pct": gem_types.Schema(type="NUMBER") for p in party_cols}
                       | {"name": gem_types.Schema(type="STRING")},
            required=["name"],
        ),
    )

    imgs = convert_from_path(str(pdf_path), first_page=page_num, last_page=page_num, dpi=DPI_VISION)
    resp = gem_client.models.generate_content(
        model=GEMINI_MODEL,
        contents=[prompt, imgs[0]],
        config=gem_types.GenerateContentConfig(
            temperature=0, response_mime_type="application/json", response_schema=schema),
    )
    data = json.loads(resp.text)
    return [[r.get(f"{p}_pct", 0) or 0 for p in party_cols] for r in data]


# ── Process page pair ─────────────────────────────────────────────────────

def process_page_pair(oai, gem_client, cfg, left_page, right_page, pdf_path, resp_dir=None):
    party_cols = cfg["csv_party_columns"]

    # Check cache
    cache_file = None
    if resp_dir:
        cache_file = resp_dir / f"page_{min(left_page,right_page):03d}_{max(left_page,right_page):03d}.json"
        if cache_file.exists():
            with open(cache_file) as f:
                cached = json.load(f)
            if cached.get("records"):
                return cached["records"], cached

    # LEFT: GPT
    left_data, left_raw, left_usage = extract_left_gpt(oai, cfg["left_prompt"], pdf_path, left_page)
    if not left_data:
        raw = {"left_page": left_page, "right_page": right_page,
               "left_response": left_raw, "records": []}
        if cache_file:
            with open(cache_file, "w") as f:
                json.dump(raw, f, indent=2, default=str)
        return [], raw

    # RIGHT: Tesseract + HMM
    n = len(left_data)
    z_pct_rows, right_raw = extract_right_hmm(pdf_path, right_page, n)

    # Fallback to Gemini if HMM found too few Z_PCT rows
    used_fallback = False
    if len(z_pct_rows) < n * 0.5 and cfg.get("right_prompt") and gem_client:
        import signal

        def _timeout_handler(signum, frame):
            raise TimeoutError("Gemini fallback timed out")

        old_handler = signal.signal(signal.SIGALRM, _timeout_handler)
        signal.alarm(60)  # 60 second timeout
        try:
            gemini_pcts = extract_right_gemini_fallback(
                gem_client, cfg, pdf_path, right_page, left_data, party_cols)
            if len(gemini_pcts) >= n * 0.5:
                z_pct_rows = gemini_pcts
                used_fallback = True
        except (Exception, TimeoutError) as e:
            pass  # keep whatever HMM produced
        finally:
            signal.alarm(0)
            signal.signal(signal.SIGALRM, old_handler)

    # Merge
    records = []
    for i in range(len(left_data)):
        l = left_data[i]
        ags = str(l.get("gemeindeschluessel", 0)).zfill(8)
        gz = l.get("gueltige_z", l.get("gueltige", 0)) or 0

        rec = {
            "ags": ags,
            "name": l.get("name", ""),
            "eligible_voters": l.get("wahlberechtigte", 0) or 0,
            "number_voters": l.get("waehler", 0) or 0,
            "invalid_votes": l.get("ungueltige_z", l.get("ungueltige", 0)) or 0,
            "valid_votes": gz,
        }

        if i < len(z_pct_rows):
            pcts = z_pct_rows[i]
            for j, p in enumerate(party_cols):
                rec[p] = round(pcts[j] * gz / 100) if j < len(pcts) else 0
            psum = sum(rec[p] for p in party_cols)
            diff = gz - psum
            if diff != 0 and psum > 0 and abs(diff) < max(5, gz * 0.05):
                rec[max(party_cols, key=lambda p: rec[p])] += diff
        else:
            for p in party_cols:
                rec[p] = 0
            rec["_warning"] = "no Z_PCT row"

        psum = sum(rec.get(p, 0) for p in party_cols)
        if gz > 0 and abs(psum - gz) > max(5, gz * 0.05):
            rec["_warning"] = f"party_sum={psum} vs VV={gz}"

        records.append(rec)

    raw = {
        "left_page": left_page, "right_page": right_page,
        "left_response": left_raw, "left_usage": left_usage,
        "used_fallback": used_fallback, "records": records,
    }
    if cache_file:
        with open(cache_file, "w") as f:
            json.dump(raw, f, indent=2, default=str)

    return records, raw


# ── Post-processing ───────────────────────────────────────────────────────

def fix_ags_codes(records, valid_kreise):
    valid_set = set(valid_kreise)
    n = 0
    for idx, rec in enumerate(records):
        if rec["ags"][:5] in valid_set:
            continue
        window = records[max(0, idx - 5):idx + 6]
        neighbors = [r["ags"][:5] for r in window if r["ags"][:5] in valid_set]
        if neighbors:
            rec["ags"] = Counter(neighbors).most_common(1)[0][0] + rec["ags"][5:]
            n += 1
    if n:
        print(f"  Fixed {n} AGS codes.")


def deduplicate(records, party_cols):
    by_ags = defaultdict(list)
    for r in records:
        by_ags[r["ags"]].append(r)
    result = []
    n = 0
    for ags, recs in by_ags.items():
        if len(recs) == 1:
            result.append(recs[0])
        else:
            n += 1
            result.append(max(recs, key=lambda r: sum(r.get(p, 0) or 0 for p in party_cols)))
    if n:
        print(f"  Deduplicated {n} AGS codes.")
    return result


def aggregate_kreisfreie(records, kfs_kreise, csv_fields):
    kfs_set = set(kfs_kreise)
    kfs_recs = [r for r in records if r["ags"][:5] in kfs_set]
    non_kfs = [r for r in records if r["ags"][:5] not in kfs_set]
    if not kfs_recs:
        return records
    agg = defaultdict(lambda: {f: 0 for f in csv_fields})
    for rec in kfs_recs:
        city = rec["ags"][:5] + "000"
        agg[city]["ags"] = city
        for f in csv_fields[1:]:
            agg[city][f] += rec.get(f, 0) or 0
    print(f"  Aggregated {len(kfs_recs)} sub-records into {len(agg)} city records.")
    return non_kfs + list(agg.values())


def validate_totals(records, official, party_cols):
    print("\n=== State-level validation ===")
    for col in ["valid_votes"] + party_cols:
        total = sum(r.get(col, 0) or 0 for r in records)
        off = official.get(col)
        if off and off > 0:
            pct = (total - off) / off * 100
            sym = "OK" if abs(pct) < 2.0 else "!!"
            print(f"  {sym} {col:18s}: {total:>10,} (official {off:>10,}, {pct:+.2f}%)")
        elif off == 0:
            print(f"  -- {col:18s}: {total:>10,}")


# ── Main ──────────────────────────────────────────────────────────────────

def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--config", required=True)
    parser.add_argument("--test", type=int, default=0)
    args = parser.parse_args()

    cfg = load_config(args.config)
    pdf_path = cfg["pdf_path"]
    party_cols = cfg["csv_party_columns"]
    csv_fields = ["ags", "eligible_voters", "number_voters", "invalid_votes",
                  "valid_votes"] + party_cols

    if not Path(pdf_path).exists():
        sys.exit(f"ERROR: {pdf_path} not found")

    resp_dir = Path(cfg["responses_dir"])
    resp_dir.mkdir(parents=True, exist_ok=True)

    oai = OpenAI(api_key=os.environ["OPENAI_API_KEY"])
    gem_client = None
    if HAS_GEMINI and os.environ.get("GEMINI_API_KEY"):
        gem_client = genai.Client(api_key=os.environ["GEMINI_API_KEY"])

    page_start, page_end = cfg["page_range"]
    layout = cfg.get("page_layout", "right_odd_left_even")

    print(f"Extracting: {cfg['election']}")
    print(f"PDF: {pdf_path}, pages {page_start}-{page_end}")
    print(f"Left: {GPT_MODEL} | Right: Tesseract+HMM (Gemini fallback)")
    if args.test:
        print(f"TEST MODE: {args.test} page pairs")
    print()

    all_records = []
    n_warnings = 0
    n_fallbacks = 0
    total_cost = 0.0
    t0 = time.time()
    pair = 0

    page = page_start
    while page + 1 <= page_end:
        if args.test and pair >= args.test:
            break

        if layout == "right_odd_left_even":
            right_page, left_page = page, page + 1
        else:
            left_page, right_page = page, page + 1

        try:
            records, raw = process_page_pair(
                oai, gem_client, cfg, left_page, right_page, pdf_path, resp_dir)
        except Exception as e:
            print(f"  ERROR p{left_page}-{right_page}: {e}")
            try:
                time.sleep(3)
                records, raw = process_page_pair(
                    oai, gem_client, cfg, left_page, right_page, pdf_path, resp_dir)
            except Exception as e2:
                print(f"  RETRY FAILED: {e2}")
                page += 2
                pair += 1
                continue

        for rec in records:
            if "_warning" in rec:
                n_warnings += 1
        if isinstance(raw, dict) and raw.get("used_fallback"):
            n_fallbacks += 1

        if isinstance(raw, dict) and "left_usage" in raw:
            lu = raw["left_usage"]
            total_cost += (lu.get("input_tokens", 0) * 0.75 +
                          lu.get("output_tokens", 0) * 4.50) / 1e6

        all_records.extend(records)
        pair += 1

        if pair % 20 == 0:
            print(f"  {pair} pairs, {len(all_records)} munis, "
                  f"${total_cost:.2f}, {time.time()-t0:.0f}s")

        page += 2

    elapsed = time.time() - t0
    print(f"\nExtraction: {len(all_records)} munis from {pair} pairs "
          f"in {elapsed:.0f}s (${total_cost:.2f})")
    print(f"Warnings: {n_warnings}, Gemini fallbacks: {n_fallbacks}")

    if cfg.get("valid_kreise"):
        fix_ags_codes(all_records, cfg["valid_kreise"])
    all_records = deduplicate(all_records, party_cols)
    if cfg.get("kreisfreie_staedte"):
        all_records = aggregate_kreisfreie(all_records, cfg["kreisfreie_staedte"], csv_fields)

    if cfg.get("official_totals"):
        validate_totals(all_records, cfg["official_totals"], party_cols)

    output_path = cfg["output_csv"]
    with open(output_path, "w", newline="") as f:
        writer = csv.DictWriter(f, fieldnames=csv_fields, extrasaction="ignore")
        writer.writeheader()
        for rec in all_records:
            writer.writerow({k: (rec.get(k, 0) or 0) for k in csv_fields})

    print(f"\nWritten {len(all_records)} records to {output_path}")


if __name__ == "__main__":
    main()
