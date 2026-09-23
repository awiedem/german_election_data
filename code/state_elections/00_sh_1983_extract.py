#!/usr/bin/env python3
"""Verify the reviewed SH 1983 transcription used by the state pipeline.

The former automatic text-layer extractor corrupted municipality codes and
counts. Its raw CSV is retained unchanged for provenance but is no longer used.
The checked transcription is in data/state_elections/derived/sh_1983; see its
README for the source, recovery method, and the postal-vote limitation.
This entry point is read-only. It does not rerun OCR or overwrite raw inputs.
"""
from pathlib import Path
import runpy

if __name__ == "__main__":
    runpy.run_path(str(Path(__file__).resolve().parents[1] / "checks/check_sh_1983_source.py"),
                  run_name="__main__")
