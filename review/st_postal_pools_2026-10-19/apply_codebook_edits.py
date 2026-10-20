"""Apply the Sachsen-Anhalt 1990-2006 postal-pool edits to docs/codebook.md and .qmd.

main's codebook holds unreleased mayoral edits, so a line-based patch does not
apply there; these exact-sentence replacements do. Each old sentence must occur
exactly once, else nothing is written. Line endings are kept as they are.

Run from the repository root:  python3 review/st_postal_pools_2026-10-19/apply_codebook_edits.py
"""
import io

OLD1 = ("in Sachsen-Anhalt 2011 and 2016, whose pooled districts name the Gemeinden or Verbandsgemeinde they serve, "
        "to exactly those (`data/state_elections/metadata/st_postal_pool_members.csv`), each weighted by its electorate "
        "in the pool's Landtagswahlkreis, since some pools serve only part of a Gemeinde.")
NEW1 = ("in Sachsen-Anhalt to Gemeinde parts of the pool's Landtagswahlkreis, each weighted by its electorate there, since "
        "some pools serve only part of a Gemeinde: in 2006, 2011 and 2016, whose pooled districts name the Gemeinden or "
        "the Verwaltungs- or Verbandsgemeinde they serve, to exactly those "
        "(`data/state_elections/metadata/st_postal_pool_members.csv`; 2006 Verwaltungsgemeinschaft members from the "
        "Gemeindeverzeichnis of 31.12.2005, updated to election day, without the Gemeinden that count their own postal "
        "votes; 14 Gemeinde parts "
        "that no pool names share the pools of their Wahlkreis at their average rate), in 1990{dash}2002, whose pools "
        "name only the Kreis or the Land, to the Kreis's part of the Wahlkreis (the whole Wahlkreis where the pool is "
        "booked on the Land or a Kreis of the Wahlkreis has no pool of its own).")
OLD2 = ("Sachsen-Anhalt spread its 2011 and 2016 pools over the whole Kreis (Quedlinburg, which counts its own postal "
        "votes, received part of Ballenstedt's) and dropped Brücken-Hackpfüffel's 2011 postal district.")
NEW2 = ("Sachsen-Anhalt spread its pools over the whole Kreis, and those booked on the Land in 1990 and 1994 over the "
        "whole state (Quedlinburg, which counts its own postal votes, received part of Ballenstedt's in 2011; 26 "
        "Gemeinden that count their own received 8,947 pooled voters in 2006), and dropped Brücken-Hackpfüffel's 2011 "
        "postal district.")
OLD3, NEW3 = "(8 receivers capped)", "(6 receivers capped)"

for path, dash in (("docs/codebook.md", "–"), ("docs/codebook.qmd", "--")):
    text = io.open(path, encoding="utf-8", newline="").read()
    for old in (OLD1, OLD2, OLD3):
        n = text.count(old)
        if n != 1:
            raise SystemExit(f"{path}: expected one occurrence, found {n}: {old[:70]}...")
    text = text.replace(OLD1, NEW1.format(dash=dash)).replace(OLD2, NEW2).replace(OLD3, NEW3)
    io.open(path, "w", encoding="utf-8", newline="").write(text)
    print("updated", path)
