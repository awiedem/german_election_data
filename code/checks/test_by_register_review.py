#!/usr/bin/env python3
"""Scoped tests for reviewed Bavarian entry repairs and restricted evidence joins."""
import copy
import datetime as dt
import importlib.util
import json
from pathlib import Path
import unittest

ROOT = Path(__file__).resolve().parents[2]


def load(name, filename):
    spec = importlib.util.spec_from_file_location(name, ROOT / "code/mayoral_elections" / filename)
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


parser = load("register_parser", "00_by_verzeichnis_parse.py")
review = load("register_review", "00c_by_register_review.py")


@unittest.skipUnless(review.INPUT.exists(), "Restricted checkpoint 417 bundle is local only")
class ReviewTests(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.bundle = json.loads(review.INPUT.read_text())
        # the page-125 entry's printed name, from the gitignored pin file (the
        # register names are restricted: LfStat, 16.10.2026)
        cls.fixed_name = next(p["name_raw"] for p in parser.load_name_pins("entry_fix_2014")
                              if p["pdf_page"] == "125")

    def entries(self):
        rows = copy.deepcopy(self.bundle["frozen_reviewed_entries"])
        for r in rows:
            r["pdf_page"] = int(r["pdf_page"])
            r["birth_year"] = int(r["birth_year"])
            r["share"] = float(r["share"]) if r["share"] else None
            for field in ["wahltag", "amtsantritt_printed"]:
                r[field] = dt.date.fromisoformat(r[field])
        return rows

    def test_only_reviewed_missing_fields_change_and_reapplication_is_safe(self):
        rows = self.entries()
        before = copy.deepcopy(rows)
        parser.apply_reviewed_entry_corrections(rows)
        changes = [(r["pdf_page"], {k for k in r if r[k] != b[k]}) for r, b in zip(rows, before) if r != b]
        self.assertCountEqual(changes, [(125, {"schl_nr", "share"}), (68, {"share"})])
        once = copy.deepcopy(rows)
        parser.apply_reviewed_entry_corrections(rows)
        self.assertEqual(rows, once)

    def test_duplicate_entry_is_rejected(self):
        rows = self.entries()
        rows.append(copy.deepcopy(next(r for r in rows if r["name_raw"] == self.fixed_name)))
        with self.assertRaisesRegex(ValueError, "no longer unique"):
            parser.apply_reviewed_entry_corrections(rows)

    def test_conflicting_share_is_not_overwritten(self):
        rows = self.entries()
        next(r for r in rows if r["name_raw"] == self.fixed_name)["share"] = 40.0
        with self.assertRaisesRegex(ValueError, "correction conflicts"):
            parser.apply_reviewed_entry_corrections(rows)

    def test_wrong_election_is_rejected(self):
        rows = self.entries()
        next(r for r in rows if r["name_raw"] == self.fixed_name)["wahltag"] = dt.date(2008, 3, 2)
        with self.assertRaisesRegex(ValueError, "anchor changed"):
            parser.apply_reviewed_entry_corrections(rows)

    def test_current_evidence_preserves_precision_conflicts_and_identity_hold(self):
        result = review.build(self.bundle, review.BASE / "raw/bayern")
        self.assertEqual(len(result["observations"]), 25)
        self.assertEqual(len(result["legacy_map_comparison"]), 53)
        self.assertTrue(all(o["birth_precision"] == "year" and "birth_date" not in o
                            for o in result["observations"]))
        self.assertEqual(len(result["birth_conflicts"]), 5)
        self.assertTrue(all(c["status"] == "unresolved" and c["adjudicated_birth_date"] is None
                            and c["adjudicated_birth_year"] is None for c in result["birth_conflicts"]))
        self.assertEqual(sum(len(c["supported_alternatives"]) for c in result["birth_conflicts"]), 10)
        self.assertEqual(result["identity_caveats"][0]["status"], "unresolved")
        self.assertTrue(result["validation"]["pforzen_exclusion_preserved"])

    def test_old_id_cannot_override_election_anchor(self):
        o = copy.deepcopy(self.bundle["observations"][0])
        o["audit_person_key"] = "p_09_wrong_legacy_id"
        panel = review.read_csv(review.BASE / "final/mayor_panel.csv")
        candidates = review.read_csv(review.RESTRICTED_CANDIDATES)
        registry = review.read_csv(review.BASE / "metadata/person_id_registry.csv")
        resolved = review.resolve_anchor(o, panel, candidates, registry)
        self.assertNotEqual(resolved["person_id"], o["audit_person_key"])
        o["printed_name"] = o["printed_name"].split()[0] + " WrongGivenName"
        with self.assertRaisesRegex(ValueError, "Name anchor changed"):
            review.resolve_anchor(o, panel, candidates, registry)
        o = copy.deepcopy(self.bundle["observations"][0])
        o["printed_share"] += 1
        with self.assertRaisesRegex(ValueError, "Share anchor changed"):
            review.resolve_anchor(o, panel, candidates, registry)


if __name__ == "__main__":
    unittest.main()
