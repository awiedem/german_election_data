#!/usr/bin/env python3
"""Regression tests for errors that previously passed NRW's arithmetic checks.

Run from any directory with Python's standard library and Rscript installed.
Fixtures are disposable copies; no raw sources or release files are written.
"""
import csv
import importlib.util
from pathlib import Path
import shutil
import subprocess
import sys
import tempfile
import unittest

sys.dont_write_bytecode = True
ROOT = Path(__file__).resolve().parents[2]
spec = importlib.util.spec_from_file_location(
    "nrw_verified", ROOT / "code/state_elections/00_nrw_1966_1970_verified.py")
verified = importlib.util.module_from_spec(spec)
spec.loader.exec_module(verified)
REFERENCE = verified.DERIVED


def read_table(path, delimiter="\t"):
    with path.open(encoding="utf-8", newline="") as stream:
        return list(csv.DictReader(stream, delimiter=delimiter))


class VerifiedNRWTest(unittest.TestCase):
    def setUp(self):
        self.temporary = tempfile.TemporaryDirectory()
        self.addCleanup(self.temporary.cleanup)
        self.directory = Path(self.temporary.name)
        for filename in ("table2_transcription.tsv", "unit_identifiers.tsv",
                         "nrw_1966_1970_kreis.csv"):
            shutil.copyfile(REFERENCE / filename, self.directory / filename)
        verified.DERIVED = self.directory
        self.addCleanup(setattr, verified, "DERIVED", REFERENCE)
        # Freeze the published ordering before mutating the identifier map.
        # The build separately verifies this ordering against the hashed raw CSV.
        ids = read_table(REFERENCE / "unit_identifiers.tsv")
        self.legacy = self.directory / "legacy.csv"
        verified.write_table(self.legacy, [
            {k: row[k] for k in ("election_year", "name", "type")} for row in ids])

    def edit(self, filename, update, delimiter="\t"):
        path = self.directory / filename
        rows = read_table(path, delimiter)
        update(rows)
        verified.write_table(path, rows, delimiter)

    def test_checked_inputs_pass(self):
        rows, checks = verified.load_verified()
        units = verified.load_units(rows, self.legacy)
        self.assertEqual((len(rows), len(checks), len(units)), (214, 800, 185))
        self.assertEqual((units[0]["ags"], units[0]["name"]), ("05001000", "Aachen"))

    def test_swapped_published_ids_fail(self):
        def swap(rows):
            rows[0]["ags"], rows[1]["ags"] = rows[1]["ags"], rows[0]["ags"]
            rows[0], rows[1] = rows[1], rows[0]
        self.edit("unit_identifiers.tsv", swap)
        rows, _ = verified.load_verified()  # All 800 arithmetic checks still pass.
        with self.assertRaisesRegex(ValueError, "Published identifier changed"):
            verified.load_units(rows, self.legacy)

    def test_wrong_code_semantics_fail(self):
        self.edit("unit_identifiers.tsv",
                  lambda rows: rows[0].update(geographic_level="municipality"))
        rows, _ = verified.load_verified()
        with self.assertRaisesRegex(ValueError, "Identifier semantics"):
            verified.load_units(rows, self.legacy)

    def test_wrong_source_pdf_fails(self):
        self.edit("table2_transcription.tsv",
                  lambda rows: rows[0].update(source_pdf="wrong.pdf"))
        with self.assertRaisesRegex(ValueError, "Source PDF"):
            verified.load_verified()

    def test_wrong_consistent_page_pair_fails(self):
        self.edit("table2_transcription.tsv", lambda rows: rows[0].update(
            pdf_left="126", pdf_right="127", printed_left="124", printed_right="125"))
        with self.assertRaisesRegex(ValueError, "Page reference"):
            verified.load_verified()

    def run_r_helper(self):
        return subprocess.run([
            "Rscript", "--vanilla", "-e",
            'source("code/state_elections/nrw_verified.R"); '
            'a <- commandArgs(TRUE); x <- gerda_nrw_verified_results(a[1], a[2]); '
            'stopifnot(sum(vapply(x, nrow, integer(1))) == 185L)',
            str(self.directory / "nrw_1966_1970_kreis.csv"), str(self.directory),
        ], cwd=ROOT, capture_output=True, text=True)

    def test_r_helper_accepts_checked_intermediate(self):
        result = self.run_r_helper()
        self.assertEqual(result.returncode, 0, result.stderr)

    def test_r_helper_rejects_misassigned_counts(self):
        def swap(rows):
            for field in verified.COUNTS:
                rows[0][field], rows[1][field] = rows[1][field], rows[0][field]
        self.edit("nrw_1966_1970_kreis.csv", swap, ",")
        result = self.run_r_helper()
        self.assertNotEqual(result.returncode, 0)
        self.assertIn("reference", result.stderr)

    def test_r_helper_rejects_swapped_ids(self):
        def swap(rows):
            rows[0]["ags"], rows[1]["ags"] = rows[1]["ags"], rows[0]["ags"]
            rows[0], rows[1] = rows[1], rows[0]
        self.edit("nrw_1966_1970_kreis.csv", swap, ",")
        result = self.run_r_helper()
        self.assertNotEqual(result.returncode, 0)
        self.assertIn("identifiers", result.stderr)


if __name__ == "__main__":
    unittest.main()
