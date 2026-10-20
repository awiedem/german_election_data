"""Codebook edit for the 2026-10-20 follow-up: VG Laucha-Schwarzeiche (Delitz am Berge,
Schafstädt) pinned to the Bad Lauchstädt pool of the Sachsen-Anhalt Landtagswahl 2006.

main's docs/codebook.md and .qmd hold unreleased mayoral edits, so a line-based patch may
not apply there; this exact-sentence replacement does. The old phrase must occur exactly
once, else nothing is written. Line endings are kept as they are.

Run from the repository root:  python3 review/st_postal_pools_2026-10-19/apply_codebook_edits_laucha.py
"""
import io

OLD = "14 Gemeinde parts that no pool names share the pools of their Wahlkreis at their average rate"
NEW = ("VG Laucha-Schwarzeiche, which no pool names, in the Bad Lauchstädt pool, whose size implies its "
       "postal voters; 12 other Gemeinde parts that no pool names share the pools of their Wahlkreis at "
       "their average rate")

for path in ("docs/codebook.md", "docs/codebook.qmd"):
    text = io.open(path, encoding="utf-8", newline="").read()
    n = text.count(OLD)
    if n != 1:
        raise SystemExit(f"{path}: expected one occurrence, found {n}")
    io.open(path, "w", encoding="utf-8", newline="").write(text.replace(OLD, NEW))
    print("updated", path)
