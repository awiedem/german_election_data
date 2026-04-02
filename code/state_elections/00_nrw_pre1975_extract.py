#!/usr/bin/env python3
"""
00_nrw_pre1975_extract.py
Extract NRW pre-1975 Landtagswahl results at Kreis / kreisfreie-Stadt level.
Uses Table 2 from each year's official PDF (kreisfreie Städte und Kreise).

Approach:
  1. Render each page at 300 DPI, run Tesseract TSV for word bounding-boxes.
  2. Detect data rows by y-position density (not garbled Lfd. Nr. values).
  3. Pair rows into (a=total, b=Briefwahl), keep only "a" rows.
  4. Assign Lfd. Nr. sequentially from known geographic unit list.
  5. Parse values using count/percentage alternation pattern.

Output: data/state_elections/raw/Landtagswahlen/Nordrhein-Westfalen/nrw_pre1975_kreis.csv
"""

import os, re, csv, sys
from pathlib import Path
from pdf2image import convert_from_path
import pytesseract
import pdfplumber
import pandas as pd

HERE = Path(__file__).resolve().parent
RAW_DIR = HERE.parent.parent / "data" / "state_elections" / "raw" / "Landtagswahlen" / "Nordrhein-Westfalen"
OUT_PATH = RAW_DIR / "nrw_pre1975_kreis.csv"

# ══════════════════════════════════════════════════════════════════
# Geographic units — ordered exactly as they appear in Table 2.
# ══════════════════════════════════════════════════════════════════

GEO_1970 = [
    # --- Reg.-Bez. Düsseldorf ---
    (1, "Düsseldorf", "krfr"), (2, "Duisburg", "krfr"),
    (3, "Essen", "krfr"), (4, "Krefeld", "krfr"),
    (5, "Leverkusen", "krfr"), (6, "Mönchengladbach", "krfr"),
    (7, "Mülheim a.d. Ruhr", "krfr"), (8, "Neuss", "krfr"),
    (9, "Oberhausen", "krfr"), (10, "Remscheid", "krfr"),
    (11, "Rheydt", "krfr"), (12, "Solingen", "krfr"),
    (13, "Wuppertal", "krfr"),
    (14, "Kreis Dinslaken", "kreis"), (15, "Kreis Düsseldorf-Mettmann", "kreis"),
    (16, "Kreis Geldern", "kreis"), (17, "Kreis Grevenbroich", "kreis"),
    (18, "Kreis Kempen-Krefeld", "kreis"), (19, "Kreis Kleve", "kreis"),
    (20, "Kreis Moers", "kreis"), (21, "Kreis Rees", "kreis"),
    (22, "Rhein-Wupper-Kreis", "kreis"),
    (23, "Reg.-Bez. Düsseldorf", "agg"),
    (24, "davon Krfr. Städte (D)", "agg"), (25, "davon Kreise (D)", "agg"),
    # --- Reg.-Bez. Köln ---
    (26, "Bonn", "krfr"), (27, "Köln", "krfr"),
    (28, "Kreis Bergheim (Erft)", "kreis"), (29, "Kreis Euskirchen", "kreis"),
    (30, "Kreis Köln", "kreis"), (31, "Oberbergischer Kreis", "kreis"),
    (32, "Rheinisch-Bergischer Kreis", "kreis"), (33, "Rhein-Sieg-Kreis", "kreis"),
    (34, "Reg.-Bez. Köln", "agg"),
    (35, "davon Krfr. Städte (K)", "agg"), (36, "davon Kreise (K)", "agg"),
    # --- Reg.-Bez. Aachen ---
    (37, "Aachen", "krfr"),
    (38, "Kreis Aachen", "kreis"), (39, "Kreis Düren", "kreis"),
    (40, "Kreis Erkelenz", "kreis"), (41, "Kreis Jülich", "kreis"),
    (42, "Kreis Monschau", "kreis"), (43, "Kreis Schleiden", "kreis"),
    (44, "Selfkantkreis Geilenkirchen-Heinsberg", "kreis"),
    (45, "Reg.-Bez. Aachen", "agg"),
    (46, "davon Krfr. Städte (AC)", "agg"), (47, "davon Kreise (AC)", "agg"),
    # --- Reg.-Bez. Münster ---
    (48, "Bocholt", "krfr"), (49, "Bottrop", "krfr"),
    (50, "Gelsenkirchen", "krfr"), (51, "Gladbeck", "krfr"),
    (52, "Münster", "krfr"), (53, "Recklinghausen", "krfr"),
    (54, "Kreis Ahaus", "kreis"), (55, "Kreis Beckum", "kreis"),
    (56, "Kreis Borken", "kreis"), (57, "Kreis Coesfeld", "kreis"),
    (58, "Kreis Lüdinghausen", "kreis"), (59, "Kreis Münster", "kreis"),
    (60, "Kreis Recklinghausen", "kreis"), (61, "Kreis Steinfurt", "kreis"),
    (62, "Kreis Tecklenburg", "kreis"), (63, "Kreis Warendorf", "kreis"),
    (64, "Reg.-Bez. Münster", "agg"),
    (65, "davon Krfr. Städte (MS)", "agg"), (66, "davon Kreise (MS)", "agg"),
    # --- Reg.-Bez. Detmold ---
    (67, "Bielefeld", "krfr"),
    (68, "Kreis Bielefeld", "kreis"), (69, "Kreis Büren", "kreis"),
    (70, "Kreis Detmold", "kreis"), (71, "Kreis Halle (Westf.)", "kreis"),
    (72, "Kreis Herford", "kreis"), (73, "Kreis Höxter", "kreis"),
    (74, "Kreis Lemgo", "kreis"), (75, "Kreis Lübbecke", "kreis"),
    (76, "Kreis Minden", "kreis"), (77, "Kreis Paderborn", "kreis"),
    (78, "Kreis Warburg", "kreis"), (79, "Kreis Wiedenbrück", "kreis"),
    (80, "Reg.-Bez. Detmold", "agg"),
    (81, "davon Krfr. Städte (DT)", "agg"), (82, "davon Kreise (DT)", "agg"),
    # --- Reg.-Bez. Arnsberg ---
    (83, "Bochum", "krfr"), (84, "Castrop-Rauxel", "krfr"),
    (85, "Dortmund", "krfr"), (86, "Hagen", "krfr"),
    (87, "Hamm", "krfr"), (88, "Herne", "krfr"),
    (89, "Iserlohn", "krfr"), (90, "Lünen", "krfr"),
    (91, "Wanne-Eickel", "krfr"), (92, "Wattenscheid", "krfr"),
    (93, "Witten", "krfr"),
    (94, "Kreis Arnsberg", "kreis"), (95, "Kreis Brilon", "kreis"),
    (96, "Ennepe-Ruhr-Kreis", "kreis"), (97, "Kreis Iserlohn", "kreis"),
    (98, "Kreis Lippstadt", "kreis"), (99, "Kreis Lüdenscheid", "kreis"),
    (100, "Kreis Meschede", "kreis"), (101, "Kreis Olpe", "kreis"),
    (102, "Kreis Siegen", "kreis"), (103, "Kreis Soest", "kreis"),
    (104, "Kreis Unna", "kreis"), (105, "Kreis Wittgenstein", "kreis"),
    (106, "Reg.-Bez. Arnsberg", "agg"),
    (107, "davon Krfr. Städte (AR)", "agg"), (108, "davon Kreise (AR)", "agg"),
    # --- NRW totals ---
    (109, "Nordrhein-Westfalen", "agg"),
    (110, "davon Krfr. Städte (NRW)", "agg"), (111, "davon Kreise (NRW)", "agg"),
]

GEO_LOOKUP_1970 = {nr: (name, typ) for nr, name, typ in GEO_1970}

GEO_1966 = [
    # --- Reg.-Bez. Düsseldorf ---
    (1, "Düsseldorf", "krfr"), (2, "Duisburg", "krfr"),
    (3, "Essen", "krfr"), (4, "Krefeld", "krfr"),
    (5, "Leverkusen", "krfr"), (6, "Mönchengladbach", "krfr"),
    (7, "Mülheim a.d. Ruhr", "krfr"), (8, "Neuss", "krfr"),
    (9, "Oberhausen", "krfr"), (10, "Remscheid", "krfr"),
    (11, "Rheydt", "krfr"), (12, "Solingen", "krfr"),
    (13, "Viersen", "krfr"),  # kreisfrei until 1970
    (14, "Wuppertal", "krfr"),
    (15, "Kreis Dinslaken", "kreis"), (16, "Kreis Düsseldorf-Mettmann", "kreis"),
    (17, "Kreis Geldern", "kreis"), (18, "Kreis Grevenbroich", "kreis"),
    (19, "Kreis Kempen-Krefeld", "kreis"), (20, "Kreis Kleve", "kreis"),
    (21, "Kreis Moers", "kreis"), (22, "Kreis Rees", "kreis"),
    (23, "Rhein-Wupper-Kreis", "kreis"),
    (24, "Reg.-Bez. Düsseldorf", "agg"),
    # --- Reg.-Bez. Köln ---
    (25, "Bonn", "krfr"), (26, "Köln", "krfr"),
    (27, "Kreis Bergheim (Erft)", "kreis"), (28, "Kreis Bonn", "kreis"),
    (29, "Kreis Euskirchen", "kreis"), (30, "Kreis Köln", "kreis"),
    (31, "Oberbergischer Kreis", "kreis"), (32, "Rheinisch-Bergischer Kreis", "kreis"),
    (33, "Siegkreis", "kreis"),
    (34, "Reg.-Bez. Köln", "agg"),
    # --- Reg.-Bez. Aachen ---
    (35, "Aachen", "krfr"),
    (36, "Kreis Aachen", "kreis"), (37, "Kreis Düren", "kreis"),
    (38, "Kreis Erkelenz", "kreis"), (39, "Kreis Jülich", "kreis"),
    (40, "Kreis Monschau", "kreis"), (41, "Kreis Schleiden", "kreis"),
    (42, "Selfkantkreis Geilenkirchen-Heinsberg", "kreis"),
    (43, "Reg.-Bez. Aachen", "agg"),
    # --- Reg.-Bez. Münster ---
    (44, "Bocholt", "krfr"), (45, "Bottrop", "krfr"),
    (46, "Gelsenkirchen", "krfr"), (47, "Gladbeck", "krfr"),
    (48, "Münster", "krfr"), (49, "Recklinghausen", "krfr"),
    (50, "Kreis Ahaus", "kreis"), (51, "Kreis Beckum", "kreis"),
    (52, "Kreis Borken", "kreis"), (53, "Kreis Coesfeld", "kreis"),
    (54, "Kreis Lüdinghausen", "kreis"), (55, "Kreis Münster", "kreis"),
    (56, "Kreis Recklinghausen", "kreis"), (57, "Kreis Steinfurt", "kreis"),
    (58, "Kreis Tecklenburg", "kreis"), (59, "Kreis Warendorf", "kreis"),
    (60, "Reg.-Bez. Münster", "agg"),
    # --- Reg.-Bez. Detmold ---
    (61, "Bielefeld", "krfr"), (62, "Herford", "krfr"),
    (63, "Kreis Bielefeld", "kreis"), (64, "Kreis Büren", "kreis"),
    (65, "Kreis Detmold", "kreis"), (66, "Kreis Halle (Westf.)", "kreis"),
    (67, "Kreis Herford", "kreis"), (68, "Kreis Höxter", "kreis"),
    (69, "Kreis Lemgo", "kreis"), (70, "Kreis Lübbecke", "kreis"),
    (71, "Kreis Minden", "kreis"), (72, "Kreis Paderborn", "kreis"),
    (73, "Kreis Warburg", "kreis"), (74, "Kreis Wiedenbrück", "kreis"),
    (75, "Reg.-Bez. Detmold", "agg"),
    # --- Reg.-Bez. Arnsberg ---
    (76, "Bochum", "krfr"), (77, "Castrop-Rauxel", "krfr"),
    (78, "Dortmund", "krfr"), (79, "Hagen", "krfr"),
    (80, "Hamm", "krfr"), (81, "Herne", "krfr"),
    (82, "Iserlohn", "krfr"), (83, "Lüdenscheid", "krfr"),
    (84, "Lünen", "krfr"), (85, "Siegen", "krfr"),
    (86, "Wanne-Eickel", "krfr"), (87, "Wattenscheid", "krfr"),
    (88, "Witten", "krfr"),
    (89, "Kreis Altena", "kreis"), (90, "Kreis Arnsberg", "kreis"),
    (91, "Kreis Brilon", "kreis"), (92, "Ennepe-Ruhr-Kreis", "kreis"),
    (93, "Kreis Iserlohn", "kreis"), (94, "Kreis Lippstadt", "kreis"),
    (95, "Kreis Meschede", "kreis"), (96, "Kreis Olpe", "kreis"),
    (97, "Kreis Siegen", "kreis"), (98, "Kreis Soest", "kreis"),
    (99, "Kreis Unna", "kreis"), (100, "Kreis Wittgenstein", "kreis"),
    (101, "Reg.-Bez. Arnsberg", "agg"),
    # --- NRW totals ---
    (102, "Nordrhein-Westfalen", "agg"),
    (103, "darunter Landkreise", "agg"),
]

GEO_LOOKUP_1966 = {nr: (name, typ) for nr, name, typ in GEO_1966}

GEO_1962 = [
    # --- Reg.-Bez. Düsseldorf ---
    (1, "Düsseldorf", "krfr"), (2, "Duisburg", "krfr"),
    (3, "Essen", "krfr"), (4, "Krefeld", "krfr"),
    (5, "Leverkusen", "krfr"), (6, "Mönchengladbach", "krfr"),
    (7, "Mülheim a.d. Ruhr", "krfr"), (8, "Neuss", "krfr"),
    (9, "Oberhausen", "krfr"), (10, "Remscheid", "krfr"),
    (11, "Rheydt", "krfr"), (12, "Solingen", "krfr"),
    (13, "Viersen", "krfr"), (14, "Wuppertal", "krfr"),
    (15, "Kreis Dinslaken", "kreis"), (16, "Kreis Düsseldorf-Mettmann", "kreis"),
    (17, "Kreis Geldern", "kreis"), (18, "Kreis Grevenbroich", "kreis"),
    (19, "Kreis Kempen-Krefeld", "kreis"), (20, "Kreis Kleve", "kreis"),
    (21, "Kreis Moers", "kreis"), (22, "Kreis Rees", "kreis"),
    (23, "Rhein-Wupper-Kreis", "kreis"),
    (24, "Reg.-Bez. Düsseldorf", "agg"),
    # --- Reg.-Bez. Köln ---
    (25, "Bonn", "krfr"), (26, "Köln", "krfr"),
    (27, "Kreis Bergheim (Erft)", "kreis"), (28, "Kreis Bonn", "kreis"),
    (29, "Kreis Euskirchen", "kreis"), (30, "Kreis Köln", "kreis"),
    (31, "Oberbergischer Kreis", "kreis"), (32, "Rheinisch-Bergischer Kreis", "kreis"),
    (33, "Siegkreis", "kreis"),
    (34, "Reg.-Bez. Köln", "agg"),
    # --- Reg.-Bez. Aachen ---
    (35, "Aachen", "krfr"),
    (36, "Kreis Aachen", "kreis"), (37, "Kreis Düren", "kreis"),
    (38, "Kreis Erkelenz", "kreis"), (39, "Kreis Jülich", "kreis"),
    (40, "Kreis Monschau", "kreis"), (41, "Kreis Schleiden", "kreis"),
    (42, "Selfkantkreis Geilenkirchen-Heinsberg", "kreis"),
    (43, "Reg.-Bez. Aachen", "agg"),
    (44, "Landesteil Nordrhein", "agg"),
    # --- Reg.-Bez. Münster ---
    (45, "Bocholt", "krfr"), (46, "Bottrop", "krfr"),
    (47, "Gelsenkirchen", "krfr"), (48, "Gladbeck", "krfr"),
    (49, "Münster", "krfr"), (50, "Recklinghausen", "krfr"),
    (51, "Kreis Ahaus", "kreis"), (52, "Kreis Beckum", "kreis"),
    (53, "Kreis Borken", "kreis"), (54, "Kreis Coesfeld", "kreis"),
    (55, "Kreis Lüdinghausen", "kreis"), (56, "Kreis Münster", "kreis"),
    (57, "Kreis Recklinghausen", "kreis"), (58, "Kreis Steinfurt", "kreis"),
    (59, "Kreis Tecklenburg", "kreis"), (60, "Kreis Warendorf", "kreis"),
    (61, "Reg.-Bez. Münster", "agg"),
    # --- Reg.-Bez. Detmold ---
    (62, "Bielefeld", "krfr"), (63, "Herford", "krfr"),
    (64, "Kreis Bielefeld", "kreis"), (65, "Kreis Büren", "kreis"),
    (66, "Kreis Detmold", "kreis"), (67, "Kreis Halle (Westf.)", "kreis"),
    (68, "Kreis Herford", "kreis"), (69, "Kreis Höxter", "kreis"),
    (70, "Kreis Lemgo", "kreis"), (71, "Kreis Lübbecke", "kreis"),
    (72, "Kreis Minden", "kreis"), (73, "Kreis Paderborn", "kreis"),
    (74, "Kreis Warburg", "kreis"), (75, "Kreis Wiedenbrück", "kreis"),
    (76, "Reg.-Bez. Detmold", "agg"),
    # --- Reg.-Bez. Arnsberg ---
    (77, "Bochum", "krfr"), (78, "Castrop-Rauxel", "krfr"),
    (79, "Dortmund", "krfr"), (80, "Hagen", "krfr"),
    (81, "Hamm", "krfr"), (82, "Herne", "krfr"),
    (83, "Iserlohn", "krfr"), (84, "Lüdenscheid", "krfr"),
    (85, "Lünen", "krfr"), (86, "Siegen", "krfr"),
    (87, "Wanne-Eickel", "krfr"), (88, "Wattenscheid", "krfr"),
    (89, "Witten", "krfr"),
    (90, "Kreis Altena", "kreis"), (91, "Kreis Arnsberg", "kreis"),
    (92, "Kreis Brilon", "kreis"), (93, "Ennepe-Ruhr-Kreis", "kreis"),
    (94, "Kreis Iserlohn", "kreis"), (95, "Kreis Lippstadt", "kreis"),
    (96, "Kreis Meschede", "kreis"), (97, "Kreis Olpe", "kreis"),
    (98, "Kreis Siegen", "kreis"), (99, "Kreis Soest", "kreis"),
    (100, "Kreis Unna", "kreis"), (101, "Kreis Wittgenstein", "kreis"),
    (102, "Reg.-Bez. Arnsberg", "agg"),
    (103, "Landesteil Westfalen", "agg"),
    (104, "Nordrhein-Westfalen", "agg"),
]

GEO_LOOKUP_1962 = {nr: (name, typ) for nr, name, typ in GEO_1962}

# Column x-centers for NRW 1962 LEFT page at 300 DPI.
# Columns: EV, (Wahlschein skip), NV, (Wahlschein2 skip), turnout%, IV, VV, CDU, CDU%
COLS_LEFT_1962 = [
    ("eligible_voters", 1001),
    ("_skip1",          1181),  ("_skip1_pct", -1),   # Wahlschein — skip
    ("number_voters",   1360),
    ("_skip2",          1522),  ("_skip2_pct", -1),   # Wahlschein2 — skip
    ("_turnout_pct",    1658),                         # turnout % — skip
    ("invalid_votes",   1820),
    ("valid_votes",     1991),
    ("col1",            2165),  ("col1_pct", 2289),   # CDU
]

# Column x-centers for NRW 1962 RIGHT page at 300 DPI.
# Party order: SPD, FDP, Zentrum, DG, DFU, GDP, UAP, Parteilose
COLS_RIGHT_1962 = [
    ("col1",   270),   ("col1_pct",  420),   # SPD
    ("col2",   555),   ("col2_pct",  685),   # FDP
    ("col3",   785),   ("col3_pct",  900),   # Zentrum
    ("col4",  1030),   ("col4_pct", 1140),   # DG
    ("col5",  1270),   ("col5_pct", 1395),   # DFU
    ("col6",  1520),   ("col6_pct", 1640),   # GDP
    ("col7",  1765),   ("col7_pct", 1870),   # UAP
    ("col8",  2000),   ("col8_pct", 2100),   # Parteilose
]

# Column x-centers for NRW 1958 LEFT page at 300 DPI.
# Columns: [skip: WV, SV, WB-reg, WS], eligible_voters, number_voters, turnout%, invalid, valid
COLS_LEFT_1958 = [
    ("_skip_wv",          1031),   # Wählerverzeichnis insgesamt — skip
    ("_skip_sv",          1196),   # Sperrvermerk — skip
    ("_skip_wb",          1376),   # Wahlberechtigte (register-based) — skip
    ("_skip_ws",          1522),   # Wahlschein — skip
    ("eligible_voters",   1731),   # Wahlberechtigte (actual)
    ("number_voters",     1904),   # Wähler
    ("_turnout_pct",      2008),   # Wahlbeteiligung % — skip
    ("invalid_votes",     2197),   # Ungültige Stimmen
    ("valid_votes",       2352),   # Gültige Stimmen
]

# Column x-centers for NRW 1958 RIGHT page at 300 DPI.
# All 10 parties on right page. DP has no percentage column.
COLS_RIGHT_1958 = [
    ("col1",   394),   ("col1_pct",   524),   # CDU
    ("col2",   671),   ("col2_pct",   782),   # SPD
    ("col3",   903),   ("col3_pct",  1015),   # FDP
    ("col4",  1117),   ("col4_pct",  1203),   # Zentrum
    ("col5",  1289),   ("col5_pct",  1358),   # BdD
    ("col6",  1477),                            # DP (no % column)
    ("col7",  1675),   ("col7_pct",  1753),   # DRP
    ("col8",  1839),   ("col8_pct",  1908),   # DG
    ("col9",  1994),   ("col9_pct",  2064),   # DSU
    ("col10", 2150),   ("col10_pct", 2236),   # Parteilose
    ("_lfd_nr", 2310),                          # Lfd.Nr — skip
]

# Column x-centers for NRW 1966 right page at 300 DPI.
# Party order: CDU, SPD, FDP, Zentrum, UAP, FSU
COLS_RIGHT_1966 = [
    ("valid_votes", 340),
    ("col1",        530),   ("col1_pct",  670),    # CDU
    ("col2",        830),   ("col2_pct",  970),    # SPD
    ("col3",       1110),   ("col3_pct", 1260),    # FDP
    ("col4",       1420),   ("col4_pct", 1560),    # Zentrum
    ("col5",       1710),   ("col5_pct", 1840),    # UAP
    ("col6",       2000),   ("col6_pct", 2130),    # FSU
]

# ══════════════════════════════════════════════════════════════════
# Election configurations
# ══════════════════════════════════════════════════════════════════

ELECTIONS = {
    1958: {
        "pdf": "Nordrhein-Westfalen_1958_Landtagswahl.pdf",
        "page_pairs": [
            (18, 19, 1, 50),    # Düsseldorf through Münster (partial)
            (20, 21, 51, 104),  # Münster cont + Detmold + Arnsberg + NRW
        ],
        "parties_left": [],  # No parties on left page
        "parties_right": ["cdu", "spd", "fdp", "zentrum", "bdd", "dp", "drp", "dg", "dsu", "parteilose"],
        "parties": ["cdu", "spd", "fdp", "zentrum", "bdd", "dp", "drp", "dg", "dsu", "parteilose"],
        "col_defs_left": COLS_LEFT_1958,
        "col_defs_right": COLS_RIGHT_1958,
        "geo_lookup": GEO_LOOKUP_1962,  # Same geographic units as 1962
        "font_filter": "Courier",
        "no_ab_pairing": True,
        "known_totals": {
            "valid_votes": 7948178,
            "cdu": 4011419, "spd": 3115738, "fdp": 566258,
        },
    },
    1962: {
        "pdf": "Nordrhein-Westfalen_1962_Landtagswahl.pdf",
        "page_pairs": [
            (22, 23, 1, 50),    # Düsseldorf + Köln + Aachen + Nordrhein + Münster (partial)
            (24, 25, 51, 104),  # Münster cont + Detmold + Arnsberg + Westfalen + NRW
        ],
        # CDU is on left page; right page has these 8 parties
        "parties_left": ["cdu"],
        "parties_right": ["spd", "fdp", "zentrum", "dg", "dfu", "gdp", "uap", "parteilose"],
        "parties": ["cdu", "spd", "fdp", "zentrum", "dg", "dfu", "gdp", "uap", "parteilose"],
        "col_defs_left": COLS_LEFT_1962,
        "col_defs_right": COLS_RIGHT_1962,
        "geo_lookup": GEO_LOOKUP_1962,
        "font_filter": "Courier",
        "no_ab_pairing": True,
        "known_totals": {
            "valid_votes": 8082567,
            "cdu": 3752116, "spd": 3497179, "fdp": 553426,
        },
    },
    1966: {
        "pdf": "Nordrhein-Westfalen_1966_Landtagswahl.pdf",
        "page_pairs": [
            (26, 27, 1, 26),    # Reg.-Bez. Düsseldorf (1-24) + Bonn+Köln (25-26)
            (28, 29, 27, 52),   # Köln cont (27-34) + Aachen (35-43) + Münster (44-52)
            (30, 31, 53, 78),   # Münster cont (53-60) + Detmold (61-75) + Arnsberg start (76-78)
            (32, 33, 79, 103),  # Arnsberg cont (79-101) + NRW totals (102-103)
        ],
        "parties": ["cdu", "spd", "fdp", "zentrum", "uap", "fsu"],
        "col_defs": COLS_RIGHT_1966,
        "geo_lookup": GEO_LOOKUP_1966,
        "known_totals": {
            "valid_votes": 8842942,
            "cdu": 3786688, "spd": 4378865, "fdp": 657055,
        },
    },
    1970: {
        "pdf": "Nordrhein-Westfalen_1970_Landtagswahl.pdf",
        # (left_page, right_page, lfd_start, lfd_end) — pages are 1-indexed
        "page_pairs": [
            (26, 27, 1, 25),    # Reg.-Bez. Düsseldorf
            (28, 29, 26, 47),   # Köln + Aachen
            (30, 31, 48, 66),   # Münster
            (32, 33, 67, 82),   # Detmold
            (34, 35, 83, 111),  # Arnsberg + NRW totals
        ],
        "parties": ["spd", "cdu", "fdp", "zentrum", "uap", "dkp", "npd"],
        "geo_lookup": GEO_LOOKUP_1970,
        "known_totals": {
            "valid_votes": 8588041,
            "spd": 3956882, "cdu": 3977164, "fdp": 473280,
            "zentrum": 8588, "dkp": 77292, "npd": 93927,
        },
    },
}

# ══════════════════════════════════════════════════════════════════
# OCR helpers
# ══════════════════════════════════════════════════════════════════

def extract_tsv(pdf_path, page_num, dpi=300):
    """Render page at `dpi` and return Tesseract TSV as DataFrame."""
    images = convert_from_path(str(pdf_path), first_page=page_num,
                               last_page=page_num, dpi=dpi)
    tsv = pytesseract.image_to_data(
        images[0], lang="deu", config="--oem 3 --psm 6",
        output_type=pytesseract.Output.DATAFRAME,
    )
    w = tsv[(tsv.conf > 10) & tsv.text.notna()].copy()
    w["text"] = w["text"].astype(str).str.strip()
    w = w[w["text"] != ""]
    w["center_x"] = w["left"] + w["width"] / 2
    w["center_y"] = w["top"] + w["height"] / 2
    w["right"] = w["left"] + w["width"]
    return w


PDF_TO_DPI = 300 / 72  # Scale factor: PDF points → 300 DPI pixels


def extract_plumber(pdf_path, page_num):
    """Extract positioned words via pdfplumber, return DataFrame in 300-DPI coords."""
    with pdfplumber.open(str(pdf_path)) as pdf:
        page = pdf.pages[page_num - 1]
        words = page.extract_words(keep_blank_chars=False, x_tolerance=2, y_tolerance=2)

    if not words:
        return pd.DataFrame(columns=["left", "top", "text", "width", "center_x",
                                      "center_y", "right", "conf"])

    rows = []
    for w in words:
        left = w["x0"] * PDF_TO_DPI
        top = w["top"] * PDF_TO_DPI
        width = (w["x1"] - w["x0"]) * PDF_TO_DPI
        rows.append({
            "left": left,
            "top": top,
            "text": w["text"],
            "width": width,
            "center_x": left + width / 2,
            "center_y": top + (w["bottom"] - w["top"]) * PDF_TO_DPI / 2,
            "right": left + width,
            "conf": 90,  # dummy confidence
        })
    return pd.DataFrame(rows)


def extract_plumber_chars(pdf_path, page_num, char_gap=25, font_filter=None):
    """
    Extract words from pdfplumber by grouping individual characters.

    Some scanned PDFs have character-level text positioning where
    extract_words() fails to merge chars into words even with large
    x_tolerance.  This function reads page.chars, groups them by y and
    then by x-proximity, and returns a DataFrame identical in schema to
    extract_plumber().

    font_filter: if set (e.g. "Courier"), keep only chars whose fontname
                 contains this string (useful for PDFs with overlapping font layers).
                 When font_filter is active, space characters in the text layer
                 are used as word boundaries (more reliable than x-gap for
                 font-filtered PDFs with tight character spacing).
    """
    with pdfplumber.open(str(pdf_path)) as pdf:
        page = pdf.pages[page_num - 1]
        chars = page.chars
        if font_filter:
            chars = [c for c in chars if font_filter in c.get("fontname", "")]

    if not chars:
        return pd.DataFrame(columns=["left", "top", "text", "width",
                                      "center_x", "center_y", "right", "conf"])

    use_space_delim = font_filter is not None

    # Convert to 300-DPI coordinates; keep spaces for space-delimited mode
    items = []
    for c in chars:
        t = c.get("text", "")
        if not t:
            continue
        if not use_space_delim and t == " ":
            continue
        x0 = c["x0"] * PDF_TO_DPI
        top = c["top"] * PDF_TO_DPI
        x1 = c["x1"] * PDF_TO_DPI
        items.append((x0, top, x1, t))

    if not items:
        return pd.DataFrame(columns=["left", "top", "text", "width",
                                      "center_x", "center_y", "right", "conf"])

    # Group into rows by y (±8 DPI px)
    items.sort(key=lambda it: it[1])
    row_groups = []
    current_row = [items[0]]
    for it in items[1:]:
        if abs(it[1] - current_row[0][1]) > 8:
            row_groups.append(current_row)
            current_row = [it]
        else:
            current_row.append(it)
    row_groups.append(current_row)

    def emit_word(word_chars):
        text = "".join(c[3] for c in word_chars).strip()
        if not text:
            return None
        left = word_chars[0][0]
        top_w = sum(c[1] for c in word_chars) / len(word_chars)
        right = word_chars[-1][2]
        return {
            "left": left, "top": top_w, "text": text,
            "width": right - left,
            "center_x": (left + right) / 2,
            "center_y": top_w + 10,
            "right": right, "conf": 85,
        }

    words_out = []
    for rg in row_groups:
        rg.sort(key=lambda it: it[0])

        if use_space_delim:
            # Use space characters as word boundaries
            word_chars = []
            for it in rg:
                if it[3] == " ":
                    if word_chars:
                        w = emit_word(word_chars)
                        if w:
                            words_out.append(w)
                        word_chars = []
                else:
                    word_chars.append(it)
            if word_chars:
                w = emit_word(word_chars)
                if w:
                    words_out.append(w)
        else:
            # Original: group by x-gap
            word_chars = [rg[0]]
            for it in rg[1:]:
                gap = it[0] - word_chars[-1][2]  # x0_new - x1_prev
                if gap > char_gap:
                    w = emit_word(word_chars)
                    if w:
                        words_out.append(w)
                    word_chars = [it]
                else:
                    word_chars.append(it)
            w = emit_word(word_chars)
            if w:
                words_out.append(w)

    return pd.DataFrame(words_out)


def textlayer_fix_numeric(text):
    """Fix common misreads in embedded text layers (different from Tesseract)."""
    for old, new in [
        (";", "3"),    # semicolon = 3 in these text layers
        ("I", "1"), ("l", "1"), ("ı", "1"), ("|", "1"),
        ("n", "3"), ("\\", "1"),
        ("o", "0"), ("O", "0"),   # o/O = 0 in Courier text layers
        ("C", "0"),               # C = 0 in Courier text layers
        ("e", "6"),               # e = 6 in Courier text layers
        ("_", "-"),               # underscore = dash (party didn't compete)
    ]:
        text = text.replace(old, new)
    return text


def ocr_fix_numeric(text):
    """Fix common OCR mis-reads in numeric context."""
    for old, new in [
        ("ı", "1"), ("İ", "1"), ("|", "1"), ("!", "1"),
        ("O", "0"), ("Q", "0"),
        ("&", "8"), ("$", "8"),
        ("?", "7"),
        ("}", "5"), ("{", "6"),
        (";", "5"), (":", "3"),
        ("h", "6"), ("b", "6"), ("n", "0"),  # common digit look-alikes
    ]:
        text = text.replace(old, new)
    # In purely-numeric context
    if re.match(r"^[\dl oO,.\-SsZzTuhbn&;:]+$", text):
        for old, new in [("l", "1"), ("o", "0"), ("S", "5"), ("s", "5"),
                         ("Z", "2"), ("z", "2"), ("T", "7"), ("u", "4")]:
            text = text.replace(old, new)
    return text


def parse_int(text):
    """Parse a numeric string to int.  Return 0 for dashes / empty."""
    if not text or text.strip() in ("-", "--", "---", ".", ",", "—", "–", ""):
        return 0
    text = ocr_fix_numeric(text)
    text = re.sub(r"[^\d]", "", text)
    if not text:
        return 0
    return int(text)


def is_percentage(text):
    """Percentage values: '46,0', '7,1', '0,3', or OCR variants like '46.0', '7,ı'."""
    t = text.strip()
    # Standard: digits, comma/period, 1 digit
    if re.match(r"^-?\d{1,3}[,.]\d$", t):
        return True
    # OCR variant: comma/period + misread digit
    if re.match(r"^-?\d{1,3}[,.][\dıİl|!oO]$", t):
        return True
    # Period variant with extra digit (e.g. "46.01" garbled)
    if re.match(r"^-?\d{1,3}[,.]\d{1,2}$", t) and len(t) <= 6:
        return True
    return False


def parse_percentage(text):
    """Parse a percentage string like '46,0' or '7,1' to a float."""
    t = text.strip()
    if is_dash(t):
        return 0.0
    # Apply OCR fixes to the decimal part
    t = ocr_fix_numeric(t)
    # Replace comma with period
    t = t.replace(",", ".")
    try:
        return float(t)
    except ValueError:
        return None


def is_dash(text):
    """Check if token is a dash (zero value)."""
    return text.strip() in ("-", "--", "---", "—", "–")


# ══════════════════════════════════════════════════════════════════
# Row detection — find data rows by y-position clustering
# ══════════════════════════════════════════════════════════════════

def find_data_rows(words_df, y_header_end=700, pair_gap=50):
    """
    Find "a"-row y-positions on a page.

    Strategy:
    1. Bin words by y (20px bins).
    2. Keep bins with ≥ 4 tokens.
    3. Merge nearby rows (< 15px apart) that are the same row split across bins.
    4. Pair consecutive rows into (a, b) where gap < pair_gap.
    5. Return only the "a" row y-positions (first of each pair).
    """
    data = words_df[words_df["top"] > y_header_end].copy()
    if data.empty:
        return []

    # Bin by y (20px bins)
    data["y_bin"] = (data["top"] / 20).astype(int)
    bin_counts = data.groupby("y_bin").size()
    busy_bins = bin_counts[bin_counts >= 4].index

    # Median y per bin
    raw_ys = []
    for yb in sorted(busy_bins):
        group = data[data["y_bin"] == yb]
        raw_ys.append(group["top"].median())
    raw_ys.sort()

    # Merge nearby rows (within 15px) — same row split across bin boundaries
    row_ys = []
    i = 0
    while i < len(raw_ys):
        cluster = [raw_ys[i]]
        while i + 1 < len(raw_ys) and raw_ys[i + 1] - cluster[0] < 15:
            cluster.append(raw_ys[i + 1])
            i += 1
        row_ys.append(sum(cluster) / len(cluster))
        i += 1

    # Pair into (a, b): "a" and "b" rows are ~30px apart,
    # different entries are ~60-90px apart.
    a_rows = []
    i = 0
    while i < len(row_ys):
        a_rows.append(row_ys[i])
        if i + 1 < len(row_ys) and row_ys[i + 1] - row_ys[i] < pair_gap:
            i += 2
        else:
            i += 1
    return a_rows


# ══════════════════════════════════════════════════════════════════
# Token merging
# ══════════════════════════════════════════════════════════════════

def merge_thousands(tokens, max_gap=40):
    """
    Merge adjacent tokens that are fragments of a thousands-separated
    number (e.g. "153" + "772" → "153772").
    Each token: (left, text, width).
    Apply OCR fix BEFORE checking digit conditions.
    """
    if not tokens:
        return []
    merged = [list(tokens[0])]
    for tok in tokens[1:]:
        prev = merged[-1]
        actual_gap = tok[0] - (prev[0] + prev[2])  # tok.left - prev.right

        # Apply OCR fix FIRST, then check digits
        prev_fixed = ocr_fix_numeric(prev[1])
        tok_fixed = ocr_fix_numeric(tok[1])
        prev_digits = re.sub(r"[^\d]", "", prev_fixed)
        tok_digits = re.sub(r"[^\d]", "", tok_fixed)

        if (prev_digits and 1 <= len(prev_digits) <= 3
                and len(tok_digits) == 3 and tok_digits.isdigit()
                and 0 < actual_gap < max_gap
                and not is_percentage(prev[1]) and not is_percentage(tok[1])
                and not is_dash(prev[1]) and not is_dash(tok[1])):
            # Merge: combine the OCR-fixed text
            prev[1] = prev_fixed + tok_fixed
            prev[2] = tok[0] + tok[2] - prev[0]  # extend width
        else:
            merged.append(list(tok))
    return merged


# ══════════════════════════════════════════════════════════════════
# Right-page extraction  (party votes)
# ══════════════════════════════════════════════════════════════════

def assign_to_column(x_center, col_centers, max_dist=120):
    """Find the nearest column for a given x position."""
    best_name = None
    best_dist = max_dist
    for name, cx in col_centers:
        dist = abs(x_center - cx)
        if dist < best_dist:
            best_dist = dist
            best_name = name
    return best_name


# Column x-centers for NRW 1970 right page at 300 DPI.
# Derived from x-position cluster analysis of page 27.
# Count columns are named; percentage columns use "_pct" suffix.
# Order confirmed from PDF header: SPD, CDU, FDP, Zentrum, UAP, DKP, NPD
COLS_RIGHT_1970 = [
    ("valid_votes", 243),
    ("col1",        414),   ("col1_pct",  535),
    ("col2",        702),   ("col2_pct",  825),
    ("col3",        971),   ("col3_pct", 1085),
    ("col4",       1237),   ("col4_pct", 1325),
    ("col5",       1474),   ("col5_pct", 1579),
    ("col6",       1731),   ("col6_pct", 1835),
    ("col7",       1984),   ("col7_pct", 2087),
]

# Count columns only (the ones we want values from)
COUNT_COLS = [name for name, _ in COLS_RIGHT_1970 if not name.endswith("_pct")]
# = ["valid_votes", "col1", "col2", "col3", "col4", "col5", "col6", "col7"]

# Lfd. Nr. column is at x > 2150
LFD_NR_XMIN = 2150


def read_lfd_nr(words_df, y_center, y_band=15):
    """Try to read the Lfd. Nr. from the rightmost column of a row."""
    lfd_words = words_df[
        (words_df["top"] > y_center - y_band)
        & (words_df["top"] < y_center + y_band)
        & (words_df["left"] > LFD_NR_XMIN)
    ].sort_values("left")

    if lfd_words.empty:
        return None

    # Concatenate all tokens in the Lfd. Nr. zone
    raw = "".join(str(t) for t in lfd_words["text"].values)
    cleaned = ocr_fix_numeric(raw)
    digits = re.sub(r"[^\d]", "", cleaned)
    if digits:
        val = int(digits)
        # Sanity: Lfd. Nr. should be 1-111
        if 1 <= val <= 150:
            return val
    return None


def extract_right_page(words_df, parties, lfd_start, n_rows, col_defs=None,
                       plumber_df=None, plumber_chars_df=None):
    """
    From a right-side Table-2 page, extract vote counts using
    position-based column assignment.

    When more rows are detected than expected, uses Lfd. Nr. readings
    to identify and remove spurious rows, then assigns sequentially.

    If plumber_df/plumber_chars_df is provided, garbled rows will be
    retried using pdfplumber at the same y-position.

    Returns dict:  lfd_nr → {valid_votes, spd, cdu, ...}
    """
    if col_defs is None:
        col_defs = COLS_RIGHT_1970

    DATA_RIGHT = LFD_NR_XMIN  # exclude Lfd. Nr. column
    lfd_end = lfd_start + n_rows - 1

    a_rows = find_data_rows(words_df)
    print(f"    Detected {len(a_rows)} 'a' rows (expected {n_rows})")

    # If too many rows detected, try to remove spurious ones using Lfd. Nr.
    if len(a_rows) > n_rows:
        lfd_readings = []
        for y in a_rows:
            lfd_readings.append(read_lfd_nr(words_df, y))

        # Categorise: in-range Lfd.Nr vs uncertain (None or out-of-range)
        in_range = []
        uncertain = []
        for y, lfd in zip(a_rows, lfd_readings):
            if lfd is not None and lfd_start <= lfd <= lfd_end:
                in_range.append((y, lfd))
            else:
                uncertain.append((y, lfd))

        if len(in_range) >= n_rows:
            # Enough confirmed rows — keep only those
            filtered = sorted(in_range, key=lambda t: t[0])[:n_rows]
        else:
            # Need some uncertain rows — prefer those within the y-range
            # of confirmed rows (not header/footer noise)
            need = n_rows - len(in_range)
            if in_range:
                y_min = min(y for y, _ in in_range)
                y_max = max(y for y, _ in in_range)
            else:
                y_min, y_max = 700, 3200
            scored = []
            for y, lfd in uncertain:
                if y_min <= y <= y_max:
                    dist = 0
                else:
                    dist = min(abs(y - y_min), abs(y - y_max))
                scored.append((dist, y, lfd))
            scored.sort()
            best = [(y, lfd) for _, y, lfd in scored[:need]]
            filtered = sorted(in_range + best, key=lambda t: t[0])

        a_rows = [y for y, _ in filtered]
        print(f"    After filtering: {len(a_rows)} rows")

    # Map col1..col7 → party names
    col_to_party = {}
    col_to_party["valid_votes"] = "valid_votes"
    for i, p in enumerate(parties):
        col_to_party[f"col{i+1}"] = p

    results = {}
    for idx, y_center in enumerate(a_rows):
        lfd_nr = lfd_start + idx
        y_band = 15

        row_words = words_df[
            (words_df["top"] > y_center - y_band)
            & (words_df["top"] < y_center + y_band)
            & (words_df["left"] < DATA_RIGHT)
        ].sort_values("left")

        if row_words.empty:
            results[lfd_nr] = {"_flag": "empty row"}
            continue

        tokens = [(int(r["left"]), str(r["text"]), int(r["width"]))
                  for _, r in row_words.iterrows()]

        # Merge thousands-separated fragments
        merged = merge_thousands(tokens, max_gap=40)

        # Assign each token to a column by x-position
        # Capture BOTH count and percentage values
        col_counts = {name: None for name, _ in col_defs if not name.endswith("_pct")}
        col_pcts = {}
        for left, txt, width in merged:
            x_center = left + width / 2
            col = assign_to_column(x_center, col_defs)
            if col is None:
                continue

            if col.endswith("_pct"):
                # Parse percentage
                if is_dash(txt.strip()):
                    col_pcts[col] = 0.0
                elif is_percentage(txt.strip()):
                    pval = parse_percentage(txt)
                    if pval is not None:
                        col_pcts[col] = pval
            else:
                # Parse count
                if is_dash(txt.strip()):
                    val = 0
                elif is_percentage(txt.strip()):
                    continue  # Skip percentages in count position
                else:
                    val = parse_int(txt)

                if val is not None and col_counts[col] is None:
                    col_counts[col] = val
                elif val is not None and col_counts[col] is not None:
                    col_counts[col] = max(col_counts[col], val)

        # Build row data with percentage-based validation
        vv = col_counts.get("valid_votes") or 0
        row_data = {"valid_votes": vv}

        # Only apply percentage validation when VV is plausible (> 10K).
        # If VV is garbled, percentage validation uses the wrong base
        # and destroys correct party counts.
        use_pct_validation = vv > 10000

        for i, p in enumerate(parties):
            col_name = f"col{i+1}"
            pct_name = f"col{i+1}_pct"
            count = col_counts.get(col_name) or 0
            pct = col_pcts.get(pct_name)

            # Use percentage to validate/correct count
            # Only correct when count and percentage disagree by >5×
            if use_pct_validation and pct is not None and pct >= 0.5:
                expected_from_pct = round(vv * pct / 100)
                if count > 0 and expected_from_pct > 0:
                    ratio = count / expected_from_pct
                    if ratio > 5 or ratio < 0.2:
                        count = expected_from_pct

            row_data[p] = count

        # If row is garbled, try pdfplumber fallback at same y-position
        # Garbled = VV too small OR VV inconsistent with party sum
        party_sum_check = sum(row_data.get(p, 0) for p in parties)
        garbled = (vv < 5000 or
                   (party_sum_check > 0 and abs(vv - party_sum_check) > max(vv, 1) * 0.5))
        if garbled:
            alt = None
            # Try char-based extraction first (handles character-level text layers)
            if alt is None and plumber_chars_df is not None:
                alt = extract_row_from_plumber(
                    plumber_chars_df, y_center, parties, col_defs,
                    fix_func=textlayer_fix_numeric)
            # Then try word-based extraction
            if alt is None and plumber_df is not None:
                alt = extract_row_from_plumber(
                    plumber_df, y_center, parties, col_defs)
            if alt is not None:
                row_data = alt

        results[lfd_nr] = row_data

    return results


# ══════════════════════════════════════════════════════════════════
# Left-page extraction  (eligible voters, number voters, invalid)
# ══════════════════════════════════════════════════════════════════

def extract_left_page(words_df, lfd_start, n_rows):
    """
    From a left-side Table-2 page, extract metadata per row.
    Returns dict:  lfd_nr → {eligible_voters, number_voters, invalid_votes}
    """
    # The left page has names in the left half and numbers in the right half.
    # We detect rows the same way as the right page.
    DATA_LEFT_START = 600   # Numbers start after ~600px (names are to the left)

    a_rows = find_data_rows(words_df)
    print(f"    Detected {len(a_rows)} 'a' rows on left page (expected {n_rows})")

    if len(a_rows) > n_rows:
        a_rows = a_rows[:n_rows]

    results = {}
    for idx, y_center in enumerate(a_rows):
        lfd_nr = lfd_start + idx
        y_band = 15

        # Extract only the numeric portion (right half of left page)
        row_words = words_df[
            (words_df["top"] > y_center - y_band)
            & (words_df["top"] < y_center + y_band)
            & (words_df["left"] > DATA_LEFT_START)
        ].sort_values("left")

        if row_words.empty:
            results[lfd_nr] = {}
            continue

        tokens = [(int(r["left"]), str(r["text"]), int(r["width"]))
                  for _, r in row_words.iterrows()]
        merged = merge_thousands(tokens, max_gap=40)

        # Extract count values (skip percentages and text)
        nums = []
        for _, txt, _ in merged:
            if is_percentage(txt):
                continue
            if is_dash(txt):
                nums.append(0)
                continue
            cleaned = re.sub(r"[^\d]", "", ocr_fix_numeric(txt))
            if cleaned and len(cleaned) >= 3:  # minimum 3 digits for meaningful counts
                nums.append(int(cleaned))

        # Expected: eligible_voters, number_voters, [turnout% skipped], invalid_votes
        row_data = {}
        if len(nums) >= 3:
            row_data["eligible_voters"] = nums[0]
            row_data["number_voters"] = nums[1]
            row_data["invalid_votes"] = nums[2]
        elif len(nums) == 2:
            row_data["eligible_voters"] = nums[0]
            row_data["number_voters"] = nums[1]
        elif len(nums) == 1:
            row_data["eligible_voters"] = nums[0]

        results[lfd_nr] = row_data

    return results


# ══════════════════════════════════════════════════════════════════
# Post-processing / validation
# ══════════════════════════════════════════════════════════════════

def validate_row(row, parties):
    """Check internal consistency."""
    issues = []
    vv = row.get("valid_votes")
    if vv is None or vv == 0:
        issues.append("missing valid_votes")
        return issues

    party_sum = sum(row.get(p, 0) or 0 for p in parties)

    if party_sum > 0 and abs(vv - party_sum) > max(vv * 0.05, 50):
        issues.append(f"VV={vv} vs sum={party_sum} (diff={vv - party_sum})")

    for p in parties:
        val = row.get(p, 0) or 0
        if val > vv * 1.1:
            issues.append(f"{p}={val} > VV={vv}")

    ev = row.get("eligible_voters")
    nv = row.get("number_voters")
    if ev and nv and nv > ev * 1.05:
        issues.append(f"voters={nv} > eligible={ev}")

    return issues


def postprocess_row(row, parties, trust_vv=False):
    """Auto-fix common OCR errors.

    trust_vv: if True, never replace VV with a smaller party_sum
              (for Courier text-layer extraction where VV is reliable).
    """
    vv = row.get("valid_votes", 0) or 0
    party_sum = sum(row.get(p, 0) or 0 for p in parties)

    if vv == 0 and party_sum > 0:
        row["valid_votes"] = party_sum
        return

    # valid_votes ~10× party_sum → prepended digit
    if party_sum > 0 and vv > party_sum * 5:
        vv_str = str(vv)
        if len(vv_str) > 1:
            trimmed = int(vv_str[1:])
            if abs(trimmed - party_sum) < party_sum * 0.05:
                row["valid_votes"] = trimmed
                vv = trimmed

    # Recompute valid_votes from sum if they diverge > 5%
    new_sum = sum(row.get(p, 0) or 0 for p in parties)
    vv = row.get("valid_votes", 0) or 0
    if new_sum > 0 and abs(vv - new_sum) > max(vv * 0.05, 100):
        if trust_vv and new_sum < vv:
            pass  # VV from separate page is reliable; parties underread
        else:
            row["valid_votes"] = new_sum


# ══════════════════════════════════════════════════════════════════
# Merge Tesseract + pdfplumber extractions
# ══════════════════════════════════════════════════════════════════

def extract_row_from_plumber(plumber_df, y_center_dpi, parties, col_defs=None,
                             fix_func=None):
    """
    Extract one row of vote data from pdfplumber words at a specific
    y-position (given in 300-DPI coordinates, same as Tesseract).

    fix_func: optional character-fixing function to apply instead of
              ocr_fix_numeric (e.g. textlayer_fix_numeric for char-based
              extraction from embedded text layers).
    """
    if col_defs is None:
        col_defs = COLS_RIGHT_1970
    if fix_func is None:
        fix_func = ocr_fix_numeric

    y_band = 20  # default band for word-based pdfplumber

    # For char-based extraction, find the a-row y precisely to avoid
    # mixing a-row and b-row tokens (which can be only ~20-30px apart).
    if fix_func is not textlayer_fix_numeric:
        pass  # use default y_band
    else:
        # The Tesseract y_center is usually between the a-row (upper,
        # ~15-20px above) and b-row (lower, ~15-20px below).
        # Find non-dash chars below y_center to locate the a-row.
        nearby = plumber_df[
            (plumber_df["top"] > y_center_dpi - 35)
            & (plumber_df["top"] < y_center_dpi + 5)
            & (~plumber_df["text"].str.strip().isin(["-", "--", "—", "–"]))
        ]
        if not nearby.empty:
            a_row_y = nearby["top"].median()
            y_center_dpi = a_row_y
        else:
            y_center_dpi -= 15  # fallback: bias toward a-row
        y_band = 12  # tight band for a-row only

    row_words = plumber_df[
        (plumber_df["top"] > y_center_dpi - y_band)
        & (plumber_df["top"] < y_center_dpi + y_band)
        & (plumber_df["left"] < LFD_NR_XMIN)
    ].sort_values("left")

    if row_words.empty or len(row_words) < 3:
        return None

    tokens = [(int(r["left"]), str(r["text"]), int(r["width"]))
              for _, r in row_words.iterrows()]

    # Apply text-layer fixes to each token BEFORE merging
    tokens = [(l, fix_func(t), w) for l, t, w in tokens]

    merged = merge_thousands(tokens, max_gap=40)

    col_counts = {name: None for name, _ in col_defs if not name.endswith("_pct")}
    col_pcts = {}
    for left, txt, width in merged:
        x_center = left + width / 2
        col = assign_to_column(x_center, col_defs)
        if col is None:
            continue
        if col.endswith("_pct"):
            if is_dash(txt.strip()):
                col_pcts[col] = 0.0
            elif is_percentage(txt.strip()):
                pval = parse_percentage(txt)
                if pval is not None:
                    col_pcts[col] = pval
        else:
            if is_dash(txt.strip()):
                val = 0
            elif is_percentage(txt.strip()):
                continue
            else:
                val = parse_int(txt)
            if val is not None and col_counts[col] is None:
                col_counts[col] = val
            elif val is not None and col_counts[col] is not None:
                col_counts[col] = max(col_counts[col], val)

    vv = col_counts.get("valid_votes") or 0
    if vv < 5000:
        return None

    row_data = {"valid_votes": vv}
    for i, p in enumerate(parties):
        col_name = f"col{i+1}"
        pct_name = f"col{i+1}_pct"
        count = col_counts.get(col_name) or 0
        pct = col_pcts.get(pct_name)
        if pct is not None and vv > 0 and pct >= 0.5:
            expected_from_pct = round(vv * pct / 100)
            if count > 0 and expected_from_pct > 0:
                ratio = count / expected_from_pct
                if ratio > 5 or ratio < 0.2:
                    count = expected_from_pct
        row_data[p] = count

    # Basic consistency check
    party_sum = sum(row_data.get(p, 0) for p in parties)
    if party_sum > 0 and abs(vv - party_sum) / max(vv, 1) > 0.20:
        return None

    return row_data


# ══════════════════════════════════════════════════════════════════
# 1962 specialised extraction  (Courier font, CDU on left page,
#                                no a/b row pairing)
# ══════════════════════════════════════════════════════════════════

def find_data_rows_no_pairing(words_df, y_header_end=650, min_tokens=4):
    """
    Find data row y-positions without a/b pairing.
    Every row with enough tokens below the header zone is a data row.
    """
    data = words_df[words_df["top"] > y_header_end].copy()
    if data.empty:
        return []

    data["y_bin"] = (data["top"] / 20).astype(int)
    bin_counts = data.groupby("y_bin").size()
    busy_bins = bin_counts[bin_counts >= min_tokens].index

    raw_ys = []
    for yb in sorted(busy_bins):
        group = data[data["y_bin"] == yb]
        raw_ys.append(group["top"].median())
    raw_ys.sort()

    # Merge nearby rows (within 20px)
    row_ys = []
    i = 0
    while i < len(raw_ys):
        cluster = [raw_ys[i]]
        while i + 1 < len(raw_ys) and raw_ys[i + 1] - cluster[0] < 20:
            cluster.append(raw_ys[i + 1])
            i += 1
        row_ys.append(sum(cluster) / len(cluster))
        i += 1

    return row_ys


def extract_1962_page_side(words_df, col_defs, parties, n_rows, y_band=20):
    """
    Extract vote data from one page side using column-based assignment.
    Works for both left page (metadata + CDU) and right page (other parties).

    Returns dict:  row_index → {party: count, ...}
    """
    data_rows = find_data_rows_no_pairing(words_df)
    print(f"    Detected {len(data_rows)} rows (expected {n_rows})")

    if len(data_rows) > n_rows:
        data_rows = data_rows[:n_rows]

    # Build column-to-party mapping
    col_to_party = {}
    for name, _ in col_defs:
        if name.startswith("_") or name.endswith("_pct"):
            continue
        col_to_party[name] = name  # meta cols keep their name

    # Map col1..colN to party names
    for i, p in enumerate(parties):
        col_to_party[f"col{i+1}"] = p

    results = {}
    for idx, y_center in enumerate(data_rows):
        row_words = words_df[
            (words_df["top"] > y_center - y_band)
            & (words_df["top"] < y_center + y_band)
        ].sort_values("left")

        if row_words.empty:
            results[idx] = {}
            continue

        tokens = [(int(r["left"]), str(r["text"]), int(r["width"]))
                  for _, r in row_words.iterrows()]

        # Apply textlayer fixes
        tokens = [(l, textlayer_fix_numeric(t), w) for l, t, w in tokens]

        # Split compound tokens where "." separates count from percentage
        # e.g. "588.52,0" → "588" + "52,0"  or  "58,1.14" → "58,1" + "14"
        # Only split when one side contains "," (German decimal = percentage).
        split_tokens = []
        for left, txt, width in tokens:
            if "." in txt and "," in txt and re.search(r"\d\.\d", txt):
                parts = txt.split(".", 1)
                if len(parts) == 2 and parts[0] and parts[1]:
                    frac = len(parts[0]) / len(txt)
                    w1 = int(width * frac)
                    w2 = width - w1
                    split_tokens.append((left, parts[0], w1))
                    split_tokens.append((left + w1, parts[1], w2))
                    continue
            split_tokens.append((left, txt, width))
        tokens = split_tokens

        merged = merge_thousands(tokens, max_gap=40)

        # Second pass: merge adjacent tokens with 2-digit second and
        # very small gap (handles Courier text-layer misaligned spaces
        # like "689"+"84" → "68984" or "177"+"18" → "17718")
        merged2 = [list(merged[0])] if merged else []
        for tok in merged[1:]:
            prev = merged2[-1]
            actual_gap = tok[0] - (prev[0] + prev[2])
            prev_digits = re.sub(r"[^\d]", "", prev[1])
            tok_digits = re.sub(r"[^\d]", "", tok[1])
            if (prev_digits and 1 <= len(prev_digits) <= 3
                    and len(tok_digits) == 2 and tok_digits.isdigit()
                    and -5 <= actual_gap <= 5
                    and not is_percentage(prev[1]) and not is_percentage(tok[1])
                    and not is_dash(prev[1]) and not is_dash(tok[1])):
                prev[1] = prev[1] + tok[1]
                prev[2] = tok[0] + tok[2] - prev[0]
            else:
                merged2.append(list(tok))
        merged = merged2

        # Assign to columns
        col_counts = {}
        col_pcts = {}
        for left, txt, width in merged:
            x_center = left + width / 2
            col = assign_to_column(x_center, col_defs)
            if col is None or col.startswith("_"):
                continue

            if col.endswith("_pct"):
                if is_dash(txt.strip()):
                    col_pcts[col] = 0.0
                elif is_percentage(txt.strip()):
                    pval = parse_percentage(txt)
                    if pval is not None:
                        col_pcts[col] = pval
            else:
                if is_dash(txt.strip()):
                    val = 0
                elif is_percentage(txt.strip()):
                    continue
                else:
                    val = parse_int(txt)

                mapped_name = col_to_party.get(col, col)
                if val is not None:
                    if mapped_name not in col_counts or val > col_counts[mapped_name]:
                        col_counts[mapped_name] = val

        # Percentage-based validation for party columns
        vv = col_counts.get("valid_votes") or 0
        for i, p in enumerate(parties):
            col_name = f"col{i+1}"
            pct_name = f"col{i+1}_pct"
            count = col_counts.get(p) or 0
            pct = col_pcts.get(pct_name)
            if pct is not None and vv > 10000 and pct >= 0.5:
                expected_from_pct = round(vv * pct / 100)
                if count > 0 and expected_from_pct > 0:
                    ratio = count / expected_from_pct
                    if ratio > 5 or ratio < 0.2:
                        col_counts[p] = expected_from_pct

        results[idx] = col_counts

    return results


def extract_year_courier(year):
    """Extraction for Courier-font-filtered PDFs (no a/b pairing)."""
    cfg = ELECTIONS[year]
    pdf_path = RAW_DIR / cfg["pdf"]
    parties = cfg["parties"]
    parties_left = cfg.get("parties_left", [])
    parties_right = cfg.get("parties_right", parties)
    geo = cfg["geo_lookup"]
    font_filter = cfg["font_filter"]

    print(f"\n{'='*60}")
    print(f"NRW {year} — {cfg['pdf']}")
    print(f"{'='*60}")

    all_left = {}
    all_right = {}

    for left_pg, right_pg, lfd_start, lfd_end in cfg["page_pairs"]:
        n_rows = lfd_end - lfd_start + 1
        print(f"\n  Pages {left_pg}/{right_pg}  Lfd. {lfd_start}-{lfd_end} ({n_rows} items)")

        # Left page — Courier chars only
        print(f"  Extracting left page {left_pg} (Courier)...")
        lw = extract_plumber_chars(pdf_path, left_pg, font_filter=font_filter)
        ld = extract_1962_page_side(lw, cfg["col_defs_left"], parties_left, n_rows)
        for idx, data in ld.items():
            all_left[lfd_start + idx] = data

        # Right page — Courier chars only
        print(f"  Extracting right page {right_pg} (Courier)...")
        rw = extract_plumber_chars(pdf_path, right_pg, font_filter=font_filter)
        rd = extract_1962_page_side(rw, cfg["col_defs_right"], parties_right, n_rows)
        for idx, data in rd.items():
            all_right[lfd_start + idx] = data

    # Combine left + right
    records = []
    flagged = 0
    for nr, (name, typ) in sorted(geo.items()):
        if typ == "agg":
            continue

        row = {"name": name, "type": typ, "election_year": year}

        # Merge left page data (metadata + any left-page parties)
        if nr in all_left:
            row.update(all_left[nr])

        # Merge right page data (parties)
        if nr in all_right:
            for k, v in all_right[nr].items():
                if k not in row:
                    row[k] = v

        # Compute invalid_votes if missing
        if "invalid_votes" not in row and row.get("number_voters") and row.get("valid_votes"):
            row["invalid_votes"] = row["number_voters"] - row["valid_votes"]

        postprocess_row(row, parties, trust_vv=True)
        issues = validate_row(row, parties)
        if issues:
            row["_issues"] = "; ".join(issues)
            flagged += 1
            print(f"    ISSUE Lfd.{nr} {name}: {'; '.join(issues)}")

        records.append(row)

    # Statewide validation
    print(f"\n  --- Statewide totals ---")
    total_vv = sum(r.get("valid_votes", 0) or 0 for r in records)
    kt = cfg.get("known_totals", {})
    print(f"  Valid votes: {total_vv:>12,}  (expected: {kt.get('valid_votes', '?'):,})")
    for p in parties:
        tp = sum(r.get(p, 0) or 0 for r in records)
        pct = tp / total_vv * 100 if total_vv > 0 else 0
        exp = kt.get(p, "")
        exp_str = f"  exp={exp:,}" if exp else ""
        print(f"  {p:>10s}: {tp:>12,}  ({pct:.1f}%){exp_str}")

    print(f"\n  Rows: {len(records)}, flagged: {flagged}")
    return records


# ══════════════════════════════════════════════════════════════════
# Main  (1966/1970 standard extraction)
# ══════════════════════════════════════════════════════════════════

def extract_year(year):
    cfg = ELECTIONS[year]
    pdf_path = RAW_DIR / cfg["pdf"]
    parties = cfg["parties"]
    geo = cfg["geo_lookup"]

    print(f"\n{'='*60}")
    print(f"NRW {year} — {cfg['pdf']}")
    print(f"{'='*60}")

    all_right = {}
    all_left = {}

    for left_pg, right_pg, lfd_start, lfd_end in cfg["page_pairs"]:
        n_rows = lfd_end - lfd_start + 1
        print(f"\n  Pages {left_pg}/{right_pg}  Lfd. {lfd_start}-{lfd_end} ({n_rows} items)")

        # Right page — Tesseract with pdfplumber fallback
        col_defs = cfg.get("col_defs", COLS_RIGHT_1970)
        print(f"  OCR right page {right_pg}...")
        rw = extract_tsv(pdf_path, right_pg)
        pw = extract_plumber(pdf_path, right_pg)
        pcw = extract_plumber_chars(pdf_path, right_pg)
        rd = extract_right_page(rw, parties, lfd_start, n_rows,
                                col_defs=col_defs, plumber_df=pw,
                                plumber_chars_df=pcw)
        all_right.update(rd)

        # Left page
        print(f"  OCR left page {left_pg}...")
        lw = extract_tsv(pdf_path, left_pg)
        ld = extract_left_page(lw, lfd_start, n_rows)
        all_left.update(ld)

    # Combine
    records = []
    flagged = 0
    for nr, (name, typ) in sorted(geo.items()):
        if typ == "agg":
            continue

        row = {"name": name, "type": typ, "election_year": year}

        if nr in all_right:
            row.update(all_right[nr])
        else:
            row["_flag"] = f"missing right (Lfd.{nr})"

        if nr in all_left:
            left = all_left[nr]
            for k in ("eligible_voters", "number_voters", "invalid_votes"):
                if k in left and k not in row:
                    row[k] = left[k]

        # Compute invalid_votes if missing
        if "invalid_votes" not in row and row.get("number_voters") and row.get("valid_votes"):
            row["invalid_votes"] = row["number_voters"] - row["valid_votes"]

        postprocess_row(row, parties)
        issues = validate_row(row, parties)
        if issues:
            row["_issues"] = "; ".join(issues)
            flagged += 1
            print(f"    ISSUE Lfd.{nr} {name}: {'; '.join(issues)}")

        records.append(row)

    # Statewide validation
    print(f"\n  --- Statewide totals ---")
    total_vv = sum(r.get("valid_votes", 0) or 0 for r in records)
    kt = cfg.get("known_totals", {})
    print(f"  Valid votes: {total_vv:>12,}  (expected: {kt.get('valid_votes', '?'):,})")
    for p in parties:
        tp = sum(r.get(p, 0) or 0 for r in records)
        pct = tp / total_vv * 100 if total_vv > 0 else 0
        exp = kt.get(p, "")
        exp_str = f"  exp={exp:,}" if exp else ""
        print(f"  {p:>10s}: {tp:>12,}  ({pct:.1f}%){exp_str}")

    print(f"\n  Rows: {len(records)}, flagged: {flagged}")
    return records


def main():
    all_records = []
    for year in sorted(ELECTIONS.keys()):
        cfg = ELECTIONS[year]
        if cfg.get("no_ab_pairing"):
            records = extract_year_courier(year)
        else:
            records = extract_year(year)
        all_records.extend(records)

    if not all_records:
        print("No records!")
        return

    # Collect all party columns across years
    all_parties = set()
    for cfg in ELECTIONS.values():
        all_parties.update(cfg["parties"])
    all_parties = sorted(all_parties)

    fieldnames = ["name", "type", "election_year",
                  "eligible_voters", "number_voters", "invalid_votes", "valid_votes"]
    fieldnames.extend(all_parties)

    clean = [{k: v for k, v in r.items() if not k.startswith("_")} for r in all_records]

    print(f"\nWriting {len(clean)} records to {OUT_PATH}")
    with open(OUT_PATH, "w", newline="") as f:
        w = csv.DictWriter(f, fieldnames=fieldnames, extrasaction="ignore")
        w.writeheader()
        w.writerows(clean)
    print("Done!")


if __name__ == "__main__":
    main()
