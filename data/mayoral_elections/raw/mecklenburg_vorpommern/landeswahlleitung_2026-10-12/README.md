# Landeswahlleitung M-V: Ergebnisübersicht Direktwahlen in MV ab 2000 (Stand 10.2026)

- **Sender:** Landeswahlleitung Mecklenburg-Vorpommern (Ilona Schaffer), landeswahlleitung@wahlen.m-v.de
- **Date:** 12.10.2026, reply to our request of 07.10.2026 to the statistics library
  (statistik.bibliothek@statistik-mv.de) for the report series B VII B761
  "Direktwahl der Bürgermeister und Landräte".
- **File:** `_Ergebnisübersicht_Direktwahlen in MV ab 2000_Stand 10.2026.xlsx`, 38,788 bytes,
  SHA-256 `827e7c9143da8ba266fb3727071ad6688eac78f9b97c117cb0a78fce02d7ef64`. Gitignored (no licence stated).
- **Content:** sheet "Vorbemerkung zu den Daten" (history of the direct elections since 2000) and
  sheet "Landrats-, Bürgermeisterwahl MV": 163 candidate rows, 116 rounds (73 Landrat, 43
  Oberbürgermeister/Bürgermeister of the kreisfreie and große kreisangehörige Städte), 2000-2026.
  Per round: turnout, the leading candidates with votes and share, the elected person, Ja/Nein
  votes for a single-candidate runoff, notes. The mail adds that there is no central register of
  the mayors of the amtsfreie Gemeinden; those stay with the Kreiswahlleitungen.

## Assessment (2026-10-12)

Not ingested: every round is already in GERDA (LAIV-MV PDFs). Used as an independent check:

- 160 of 163 rows agree with GERDA exactly on date, round and votes; all 68 matched elected
  persons are GERDA's winners. Spelling differences only: Weiß/"Weiss" (Nordwestmecklenburg
  2021), a missing comma.
- **Missing in GERDA (fixed 2026-10-12):** the Wismar Bürgermeisterwahl of 12.04.2026 with runoff
  26.04.2026 (the raw folder has Wismar 2002, 2010 and 2018 only). Now transcribed from the
  city's official Bekanntmachungen in `../wismar_2026/`.
- **Wrong in GERDA (fixed 2026-10-12):** Landkreis Rügen, Landratswahl 06.05.2001 / runoff 20.05.2001. Pieper
  withdrew; Kassner stood alone in a Ja/Nein runoff and failed the 25 %-of-the-electorate
  quorum (§ 64 Abs. 3 KWG M-V), so a new election followed (02./16.09.2001). GERDA marked Kassner
  as the winner of the May cycle too (`landrat_candidates`, `is_winner = TRUE`, runoff votes
  10,589 = her Ja votes). `00_mv_parse.py` now flags a cycle whose Ja/Nein
  runoff fails the quorum (`flag_decisive_round_missing`; only this one), so the May cycle
  seats nobody.
