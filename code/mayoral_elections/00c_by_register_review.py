#!/usr/bin/env python3
"""Build restricted evidence from the visually reviewed Bavarian register checkpoint.

Run after 00_by_verzeichnis_parse.py and after any candidate/panel rebuild. The
gitignored input is a frozen evidence bundle, not a set of person-ID overrides.
Current identities come from an election anchor and the active stable registry.
No candidate, panel, original source or de_housing file is written by this script.
See docs/bayern_register_review_417.md for the reproduction and precision policy.
"""

import argparse
import csv
import hashlib
import json
import re
import unicodedata
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
BASE = ROOT / "data/mayoral_elections"
INPUT = BASE / "processed/by_register_review_417_restricted.json"
# The register names are withheld from the public candidates file (LfStat, 16.10.2026),
# so the name anchors are checked against the restricted twin.
RESTRICTED_CANDIDATES = BASE / "final_restricted/mayoral_candidates_restricted.csv"
OUTPUT = BASE / "final_restricted/register_review_417"


def read_csv(path):
    with Path(path).open(encoding="utf-8", newline="") as f:
        return list(csv.DictReader(f))  # AGS and all identifiers stay strings.


def sha256(path):
    h = hashlib.sha256()
    with Path(path).open("rb") as f:
        for block in iter(lambda: f.read(1048576), b""):
            h.update(block)
    return h.hexdigest()


def require(condition, message):
    if not condition:
        raise ValueError(message)


def one(rows, label):
    require(len(rows) == 1, f"{label}: expected one row, found {len(rows)}")
    return rows[0]


def norm(value):
    value = value.casefold().replace("ä", "ae").replace("ö", "oe").replace("ü", "ue")
    return re.sub(r"[^a-z]", "", unicodedata.normalize("NFKD", value).encode("ascii", "ignore").decode())


def resolve_anchor(anchor, panel, candidates, registry):
    ags, date = anchor["ags"], anchor["printed_election_date"]
    require(isinstance(ags, str) and re.fullmatch(r"09\d{6}", ags), "Invalid string AGS")
    c = one([r for r in candidates if r["ags"] == ags and r["is_winner"] == "TRUE"
             and (r["election_date_sw"] or r["election_date"]) == date], f"winner {ags}/{date}")
    actual_name = f'{c["candidate_last_name"]} {c["candidate_first_name"]}'
    require(norm(actual_name) == norm(anchor["printed_name"]), f"Name anchor changed: {ags}/{date}")
    share = float(c["candidate_voteshare_sw"] or c["candidate_voteshare_hw"]) * 100
    require(abs(share - anchor["printed_share"]) <= .11, f"Share anchor changed: {ags}/{date}")
    p = one([r for r in panel if r["ags"] == ags and r["election_date"] == c["election_date"]],
            f"panel {ags}/{date}")
    require(abs(float(p["winner_voteshare"]) * 100 - share) < 1e-8, "Candidate/panel share disagreement")
    stable = one([r for r in registry if r["ags"] == ags and r["election_date"] == p["election_date"]
                  and r["active"] == "TRUE"], f"active registry {ags}/{date}")
    require(stable["person_id"] == p["person_id"], f"Stable ID disagrees: {ags}/{date}")
    return {"person_id": p["person_id"], "anchor_ags": ags,
            "anchor_election_date": c["election_date"], "decisive_date": date,
            "decisive_share_percent": share, "gerda_name": actual_name}


def build(bundle, entries_dir):
    inputs = [INPUT, entries_dir / "by_verzeichnis_entries_restricted.csv",
              entries_dir / "by_verzeichnis_persons_restricted.csv", BASE / "final/mayor_panel.csv",
              RESTRICTED_CANDIDATES, BASE / "metadata/person_id_registry.csv",
              ROOT / "docs/person_id_map_2026-10-12_bayern_2014_register.csv"]
    input_hashes = {str(p.relative_to(ROOT)) if p.is_relative_to(ROOT) else str(p): sha256(p)
                    for p in inputs}
    source_hashes = {}
    for s in bundle["source_manifest"]:
        path = ROOT / s["gerda_source_path"]
        require(sha256(path) == s["sha256"], f"Original source hash changed: {path}")
        source_hashes[s["source_id"]] = s["sha256"]
    review_dir = ROOT / bundle["review_directory"]
    for source in bundle["input_files"]:
        require(sha256(review_dir / source["path"]) == source["sha256"],
                f"Frozen review changed: {source['path']}")

    entries = read_csv(entries_dir / "by_verzeichnis_entries_restricted.csv")
    persons = read_csv(entries_dir / "by_verzeichnis_persons_restricted.csv")
    panel = read_csv(BASE / "final/mayor_panel.csv")
    candidates = read_csv(RESTRICTED_CANDIDATES)
    registry = read_csv(BASE / "metadata/person_id_registry.csv")
    legacy = read_csv(ROOT / "docs/person_id_map_2026-10-12_bayern_2014_register.csv")
    require(legacy == bundle["frozen_id_map"], "Frozen and current 2014-register maps differ; review required")

    # Record all legacy map entries against today's stable election keys. Never
    # assume the intermediate IDs in this older map are still current.
    map_comparison = []
    for r in legacy:
        active = one([p for p in registry if p["ags"] == r["ags"]
                      and p["election_date"] == r["election_date"] and p["active"] == "TRUE"],
                     "legacy map election in stable registry")
        map_comparison.append(dict(r, current_stable_person_id=active["person_id"]))

    anchors, observations, reconciliation = {}, [], []
    for o in bundle["observations"] + bundle["prior_conflict_anchors"]:
        anchors[o["audit_person_key"]] = resolve_anchor(o, panel, candidates, registry)
    require(len(anchors) == 27, "Expected 25 reviewed people and two earlier conflict anchors")
    for o in bundle["observations"]:
        anchor = anchors[o["audit_person_key"]]
        e = one([r for r in entries if r["volume"] == "2014r" and r["pdf_page"] == str(o["pdf_page"])
                 and r["name_raw"] == o["printed_name"]], o["fact_id"])
        require(e["match_status"].startswith("matched") and e["ags"] == o["ags"], "Entry match changed")
        require(e["wahltag"] == o["printed_election_date"] and
                abs(float(e["share"]) - o["printed_share"]) < 1e-8, "Entry election/share mismatch")
        require(e["birth_year"] == str(o["birth_year"]) and e["birth_date"] == "", "Birth precision changed")
        require(e["amtsantritt_printed"] == o["printed_first_accession"], "Printed accession changed")
        require(o["visual_review"] and not any(o[k] for k in
                ["eligibility_assignment", "legal_category_assigned", "actual_service_boundary_assigned"]),
                "Reviewed observation is not a service or eligibility decision")
        frozen = one([r for r in bundle["frozen_reviewed_entries"] if r["pdf_page"] == str(o["pdf_page"])
                      and r["name_raw"] == o["printed_name"]], "frozen observation")
        diffs = {k: {"before": frozen[k], "after": e[k]} for k in frozen if frozen[k] != e[k]}
        allowed = {"schl_nr", "share", "match_status", "match_score", "ags", "source_gemeinde",
                   "source_share", "source_amtsantritt", "source_amt_ok"} if o["parser_correction"] else set()
        require(set(diffs) <= allowed, f"Unexpected entry changes for {o['fact_id']}: {diffs}")
        person = one([p for p in persons if p["ags"] == e["ags"]
                      and p["amtsantritt"] == e["source_amtsantritt"]], "source person key")
        record = dict(o, **anchor, birth_precision="year",
                      birth_conflict_unresolved=o["audit_person_key"] in bundle["conflict_alternatives"])
        observations.append(record)
        reconciliation.append({"fact_id": o["fact_id"], "audit_person_key": o["audit_person_key"],
                               **anchor, "entry_changes": diffs,
                               "register_only_birth_year": person["birth_year"],
                               "register_only_conflict_flag": person["flag_birth_year_conflict"],
                               "printed_first_accession": e["amtsantritt_printed"],
                               "name_variant_note": next((v["note"] for v in bundle["name_variants"]
                                                          if v["ags"] == o["ags"]), "")})

    conflicts = []
    for old_id, alternatives in bundle["conflict_alternatives"].items():
        prior = one([p for p in bundle["prior_conflict_people"] if p["audit_person_key"] == old_id],
                    "prior conflict")
        conflicts.append({"audit_person_key": old_id, **anchors[old_id],
                          "status": "unresolved", "flag_birth_year_conflict": True,
                          "adjudicated_birth_date": None, "adjudicated_birth_year": None,
                          "supported_alternatives": alternatives,
                          "prior_review_source_description": prior["birth_source"],
                          "note": "Intermediate years in a combined envelope are not independent evidence. "
                                  "Repeated editions do not settle the disagreement."})
    require(len(conflicts) == 5, "Expected five unresolved birth conflicts")
    for update in bundle["audit_patch"]["birth_updates"]:
        if update["status"] == "accepted_audit_birth_interval":
            o = one([o for o in observations if o["audit_person_key"] == update["audit_person_key"]],
                    "accepted year")
            require(str(o["birth_year"]) == update["dob_lower"][:4] == update["dob_upper"][:4],
                    "Accepted year changed")
    # Keep the current Pforzen exclusion. A later entry must never name its predecessor.
    pforzen = one([r for r in entries if r["volume"] == "2014r" and r["ags"] == "09777158"], "Pforzen")
    require(not pforzen["last_name"] and not pforzen["first_name"], "Pforzen exclusion was lost")
    identity_caveats = [dict(c, person_id=anchors[c["audit_person_key"]]["person_id"])
                        for c in bundle["identity_caveats"]]
    return {"schema_version": 1, "checkpoint": 417, "policy": bundle["policy"],
            "source_manifest": bundle["source_manifest"], "observations": observations,
            "birth_conflicts": conflicts, "identity_caveats": identity_caveats,
            "name_variants": bundle["name_variants"], "reconciliation": reconciliation,
            "legacy_map_comparison": map_comparison,
            "validation": {"all_passed": True, "observations": len(observations),
                           "current_identity_anchors": len(anchors), "birth_conflicts": len(conflicts),
                           "legacy_map_rows": len(map_comparison), "pforzen_exclusion_preserved": True,
                           "source_sha256": source_hashes,
                           "input_sha256": input_hashes}}


def write_outputs(result, output):
    output.mkdir(parents=True, exist_ok=True)
    (output / "evidence.json").write_text(json.dumps(result, ensure_ascii=False, indent=2) + "\n")
    cols = ["fact_id", "person_id", "audit_person_key", "ags", "municipality", "gerda_name",
            "printed_name", "pdf_page", "printed_election_date", "printed_share", "birth_year",
            "birth_precision", "printed_first_accession", "printed_office_category",
            "birth_conflict_unresolved", "source_id", "source_url"]
    with (output / "observations.csv").open("w", newline="", encoding="utf-8") as f:
        w = csv.DictWriter(f, fieldnames=cols, extrasaction="ignore")
        w.writeheader(); w.writerows(result["observations"])
    rows = [dict(person_id=c["person_id"], audit_person_key=c["audit_person_key"],
                 ags=c["anchor_ags"], status=c["status"], alternative=i, **a)
            for c in result["birth_conflicts"] for i, a in enumerate(c["supported_alternatives"], 1)]
    with (output / "birth_conflicts.csv").open("w", newline="", encoding="utf-8") as f:
        w = csv.DictWriter(f, fieldnames=list(rows[0]))
        w.writeheader(); w.writerows(rows)


def main():
    cli = argparse.ArgumentParser(description=__doc__)
    cli.add_argument("--entries-dir", type=Path, default=BASE / "raw/bayern")
    cli.add_argument("--output-dir", type=Path, default=OUTPUT)
    cli.add_argument("--check", action="store_true", help="Validate inputs without writing outputs")
    args = cli.parse_args()
    bundle = json.loads(INPUT.read_text())
    result = build(bundle, args.entries_dir)
    # Detect concurrent edits to any data input before emitting its derived evidence.
    for p, digest in result["validation"]["input_sha256"].items():
        require(sha256(ROOT / p) == digest, f"Input changed during build: {p}")
    if not args.check:
        write_outputs(result, args.output_dir)
    print(json.dumps({k: v for k, v in result["validation"].items() if not k.endswith("sha256")}, indent=2))


if __name__ == "__main__":
    main()
