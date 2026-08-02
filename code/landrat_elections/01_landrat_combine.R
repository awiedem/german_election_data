### Combine scraped Landrat data with existing landrat outputs from the mayoral pipeline
# Vincent Heddesheimer, May 2026
#
# Inputs:
#   data/landrat_elections/final/landrat_unharm.{rds,csv}        (from mayoral pipeline — BY, NRW, RLP, NI, SL)
#   data/landrat_elections/final/landrat_candidates.{rds,csv}    (from mayoral pipeline)
#   data/landrat_elections/raw/sachsen/                          (from 00_sn_scrape.R)
#   data/landrat_elections/raw/brandenburg/                      (from 00_bb_scrape.R)
#   data/landrat_elections/raw/sachsen_anhalt/                   (from 00_st_scrape.R)
#
# Outputs (overwrites):
#   data/landrat_elections/final/landrat_unharm.{rds,csv}
#   data/landrat_elections/final/landrat_candidates.{rds,csv}
#
# Schema matches the existing landrat outputs. Per-state parsers below.
# Run order: after 00_*_scrape.R for each state, after the mayoral pipeline.

rm(list = ls())
gc()

pacman::p_load(tidyverse, here, conflicted, rvest, xml2,
               readxl, data.table, lubridate)
conflict_prefer("filter", "dplyr"); conflict_prefer("year", "lubridate")
conflict_prefer("first", "dplyr"); conflict_prefer("between", "dplyr")
setwd(here::here())

# ============================================================================
# Helpers
# ============================================================================

# Pad numeric Kreis code to standard 8-digit AGS (state_prefix + kreis + 000)
build_ags <- function(state_prefix, kreis_code) {
  k <- str_pad(as.character(kreis_code), width = 5, side = "left", pad = "0")
  # If already 5-digit (state+kreis), append 000; if 8-digit, return as-is.
  if (nchar(k) == 5) {
    paste0(k, "000")
  } else if (nchar(k) == 8) {
    k
  } else {
    sprintf("%s%s000", state_prefix, str_pad(as.character(kreis_code),
                                              width = 3, side = "left", pad = "0"))
  }
}

# Split a candidate name field "Müller, Karl (CDU)" into name and party
split_name_party <- function(s) {
  s <- str_squish(s)
  pty <- str_match(s, "\\(([^)]+)\\)$")[, 2]
  nm  <- str_squish(sub("\\s*\\([^)]+\\)$", "", s))
  list(name = nm, party = pty)
}

parse_de_num <- function(x) {
  x <- gsub("\\.", "", x)
  x <- gsub(",", ".", x)
  x <- gsub("\\s+", "", x)
  suppressWarnings(as.numeric(x))
}

# ---- Sachsen-Anhalt anonymisation ------------------------------------------
# VERBATIM COPY of anonymise_st_losers() in
# code/mayoral_elections/01b_mayoral_candidates.R (~line 2717). THE TWO COPIES
# MUST STAY IN SYNC. It is needed here as well because this script re-binds new
# candidate rows onto the already-anonymised landrat_candidates and rewrites
# the file — so without it, re-running the combine undoes 01b's anonymisation
# for the ST Landrat rows this script's own scraper contributes (85 named
# losing candidates from 2007/2014/2015).
#
# The Statistisches Landesamt Sachsen-Anhalt supplies its candidate data for
# SCIENTIFIC USE ONLY; only anonymised data may be redistributed. Independently,
# § 80 KWO LSA bars publishing Wahlvorschlag (candidate) data more than six
# months after the final result — a source-independent statutory ground that
# also covers the rows scraped from the public Landeswahlleiter portal. The
# implemented project policy is therefore "all ST losers, regardless of source".
# Vote counts, shares, ranks and the Wahlvorschlagsträger (party) are NOT
# personal data and are kept, so the electoral analysis is unaffected. Winners
# are public office-holders and keep their names, exactly as in Bayern.
st_personal_cols <- c("candidate_name", "candidate_last_name",
                      "candidate_first_name", "candidate_title",
                      "candidate_gender", "candidate_birth_year",
                      "candidate_profession")

anonymise_st_losers <- function(df, label) {
  if (!all(c("ags", "is_winner") %in% names(df)) || !nrow(df)) return(df)
  idx <- substr(as.character(df$ags), 1, 2) == "15" & !(df$is_winner %in% TRUE)
  n_before <- sum(idx & !is.na(df$candidate_last_name) &
                    nzchar(trimws(as.character(df$candidate_last_name))))
  for (cl in intersect(st_personal_cols, names(df))) {
    df[[cl]][idx] <- NA
  }
  cat("  ", label, ": anonymised ", sum(idx), " ST non-winner rows (",
      n_before, " carried a name)\n", sep = "")
  df
}

# ============================================================================
# Sachsen-Anhalt parser (CSVs in ISO-8859-1)
# ============================================================================
parse_st <- function(raw_dir) {
  out <- list()

  # ---- ST 2015 single HTML file (Altmarkkreis Salzwedel only) ----
  html_files <- list.files(raw_dir, pattern = "^ST_2015_\\d{5}\\.html$",
                            full.names = TRUE)
  for (f in html_files) {
    sch <- str_match(basename(f), "_(\\d{5})\\.html$")[, 2]
    raw <- iconv(readLines(f, warn = FALSE), from = "ISO-8859-1", to = "UTF-8")
    pg <- read_html(paste(raw, collapse = "\n"))
    txt <- html_text(pg)
    # Date pattern "Wahl am DD.MM.YYYY"
    date_str <- str_match(txt, "Wahl\\s+am\\s+(\\d{2}\\.\\d{2}\\.\\d{4})")[, 2]
    election_date <- if (!is.na(date_str)) {
      as.Date(date_str, format = "%d.%m.%Y")
    } else NA
    # Kreis name from first table cell containing Kreis label
    kreis_name <- str_match(txt, "Altmarkkreis\\s+Salzwedel|Saalekreis|Salzlandkreis|Burgenlandkreis|Anhalt-Bitterfeld|Mansfeld-Südharz|Wittenberg|Stendal|Jerichower Land|Harz|Börde")[, 1]
    if (is.na(kreis_name)) kreis_name <- paste0("Kreis ", sch)

    # Table rows
    rows <- pg %>% html_elements("tr")
    parsed <- map_df(rows, function(tr) {
      tds <- tr %>% html_elements("td") %>% html_text(trim = TRUE)
      if (length(tds) < 3) return(tibble())
      tibble(merkmal = str_squish(tds[1]),
             zahl = parse_de_num(tds[2]),
             prozent = parse_de_num(tds[3]))
    })
    if (nrow(parsed) == 0) next

    eligible <- parsed$zahl[grepl("Wahlberechtigte", parsed$merkmal)][1]
    voters   <- parsed$zahl[grepl("W.hler", parsed$merkmal)][1]
    invalid  <- parsed$zahl[grepl("Ung.ltige", parsed$merkmal)][1]
    valid    <- parsed$zahl[grepl("G.ltige", parsed$merkmal)][1]

    # Candidate rows: have "(Party)" in merkmal, not summary
    cand_rows <- parsed %>%
      filter(grepl("\\([^)]+\\)", merkmal),
             !grepl("Wahlberechtigte|W.hler|Ung.ltig|G.ltig", merkmal))
    if (nrow(cand_rows) == 0) next

    cands <- cand_rows %>%
      transmute(
        # Strip leading " 1. " ranking prefix if present
        merkmal_clean = sub("^\\s*\\d+\\.\\s*", "", merkmal),
        sp = map(merkmal_clean, split_name_party),
        candidate_name  = map_chr(sp, "name"),
        candidate_party = map_chr(sp, "party"),
        candidate_votes = zahl,
        candidate_voteshare = prozent / 100
      ) %>% select(candidate_name, candidate_party, candidate_votes, candidate_voteshare)

    out[[length(out) + 1]] <- cands %>%
      mutate(
        ags = paste0(sch, "000"),
        ags_name = kreis_name,
        state = "15", state_name = "Sachsen-Anhalt",
        election_year = 2015,
        election_date = election_date,
        election_type = "Landratswahl",
        round = "hauptwahl",
        eligible_voters = eligible,
        number_voters = voters,
        valid_votes = valid,
        invalid_votes = invalid,
        turnout = ifelse(!is.na(eligible) & eligible > 0,
                         voters / eligible, NA_real_)
      )
  }

  # ---- Rolling current-cycle file (lr_rolling.csv, 2019 onward) ------------
  # The per-year lr{YY}dat{N}.csv scheme only ever existed for lr07 and lr14 --
  # every other year 404s -- which is why the ST Landrat series stopped at 2015
  # while the silent probe in 00_st_scrape.R reported nothing wrong. The current
  # results live in one rolling file, /wahlen/lrlr/erg/csv/lr.csv, in a
  # completely different WIDE schema: 21 metadata columns then nine 7-column
  # candidate blocks (B{k}_TITEL, _NAME, _VORNAME, _PARTABK, _GESCHL, _STI,
  # _STI_SW), carrying Hauptwahl and Stichwahl side by side.
  rolling <- file.path(raw_dir, "lr_rolling.csv")
  if (file.exists(rolling)) {
    rr <- read.csv2(rolling, fileEncoding = "ISO-8859-1", check.names = FALSE,
                    colClasses = "character", stringsAsFactors = FALSE)
    numv <- function(x) suppressWarnings(as.numeric(ifelse(trimws(x) == "", NA, x)))

    for (i in seq_len(nrow(rr))) {
      r <- rr[i, ]
      # ART == "HaOB" marks the three kreisfreie Städte (Dessau-Roßlau, Halle,
      # Magdeburg). Those are Oberbürgermeister elections and already come from
      # the StaLA mayoral source; taking them here would duplicate them into the
      # Landrat dataset.
      if (!is.na(r[["ART"]]) && grepl("OB", r[["ART"]])) next
      key <- gsub("\\D", "", r[["GNR1994"]])
      if (nchar(key) != 5) next
      ags8 <- paste0(key, "000")

      for (rnd in c("hauptwahl", "stichwahl")) {
        sw <- rnd == "stichwahl"
        datum <- as.Date(r[[if (sw) "WDATUM_SW" else "WDATUM"]], format = "%d.%m.%Y")
        if (is.na(datum)) next          # no runoff was held
        eligible <- numv(r[[if (sw) "WBER_SW"   else "WBER"]])
        voters   <- numv(r[[if (sw) "WAEHLER_SW" else "WAEHLER"]])
        invalid  <- numv(r[[if (sw) "UNGSTI_SW" else "UNGSTI"]])
        valid    <- numv(r[[if (sw) "GUESTI_SW" else "GUESTI"]])

        cands <- list()
        for (k in 1:9) {
          nm <- r[[paste0("B", k, "_NAME")]]
          if (is.null(nm) || is.na(nm) || trimws(nm) == "") next
          v <- numv(r[[paste0("B", k, "_STI", if (sw) "_SW" else "")]])
          # A candidate absent from this round has an empty vote cell; in the
          # Stichwahl that is everyone except the two who went through.
          if (is.na(v)) next
          vn <- trimws(r[[paste0("B", k, "_VORNAME")]])
          cands[[length(cands) + 1]] <- tibble(
            candidate_name = paste0(trimws(nm), if (nzchar(vn)) paste0(", ", vn) else ""),
            candidate_party = {
              p <- trimws(r[[paste0("B", k, "_PARTABK")]])
              if (nzchar(p)) p else NA_character_
            },
            candidate_votes = v
          )
        }
        if (length(cands) == 0) next

        out[[length(out) + 1]] <- bind_rows(cands) %>%
          mutate(
            ags = ags8, ags_name = trimws(r[["GNAME"]]),
            state = "15", state_name = "Sachsen-Anhalt",
            election_year = as.integer(format(datum, "%Y")),
            election_date = datum,
            election_type = "Landratswahl",
            round = rnd,
            eligible_voters = eligible,
            number_voters = voters,
            valid_votes = valid,
            invalid_votes = invalid,
            # Scalar conditions: see the note in the block above on why these
            # are if/else and not ifelse().
            turnout = if (!is.na(eligible) && eligible > 0) voters / eligible else NA_real_,
            candidate_voteshare = if (!is.na(valid) && valid > 0) {
              candidate_votes / valid
            } else NA_real_
          )
      }
    }
  }

  # ---- CSV downloads (2007, 2014) ----
  # lr_rolling.csv is excluded: it is a different schema entirely and the
  # positional triplet reader below would silently misread it.
  files <- setdiff(
    list.files(raw_dir, pattern = "\\.csv$", full.names = TRUE),
    file.path(raw_dir, "lr_rolling.csv")
  )
  if (length(files) == 0) return(if (length(out) == 0) NULL else bind_rows(out))

  # Helper: aggregate Gemeinde rows to a single Kreis-level row.
  # ST 2007 lr07dat3.csv contains ONLY 8-digit Gemeinde codes (no 5-digit
  # Kreis summary), so we sum up vote counts per (state+kreis) prefix.
  aggregate_st_gemeinden <- function(df, year_full, round) {
    rows_out <- list()
    # Detect schema: 2007 = ERGART;DATUM;3 LEER;NR(8 digit);NAME;A;B;C;D;triplets
    # 2014 = ERGART;DATUM;3 LEER;Satzart;NR;NAME;A;B;C;D;triplets
    has_satzart <- TRUE
    if (nrow(df) > 0) {
      first_col6 <- as.character(df[[6]][1])
      if (!is.na(first_col6) && grepl("^[0-9]{6,}$", first_col6)) {
        has_satzart <- FALSE  # 2007 schema
      }
    }
    nr_col <- if (has_satzart) 7 else 6
    name_col <- if (has_satzart) 8 else 7
    a_col <- if (has_satzart) 9 else 8
    triplet_start <- if (has_satzart) 13 else 12

    schl_all <- as.character(df[[nr_col]])
    # Group by 5-digit Kreis prefix (Sachsen-Anhalt = "15xxx")
    df$kreis5 <- str_pad(substr(schl_all, 1, 5), 5, "left", "0")
    valid_rows <- which(grepl("^15[0-9]{3}$", df$kreis5) &
                          nchar(schl_all) == 8)
    if (length(valid_rows) == 0) return(NULL)

    # Skip rows in kreisfreie Städte (15001 Dessau-Roßlau, 15002 Halle, 15003 Magdeburg)
    df_lk <- df[valid_rows, ]
    df_lk <- df_lk[!df_lk$kreis5 %in% c("15001", "15002", "15003"), ]
    if (nrow(df_lk) == 0) return(NULL)

    # For each Kreis: aggregate
    n_cols <- ncol(df)
    candidate_col_pairs <- list()
    for (k in 1:11) {
      n_idx <- triplet_start + (k - 1) * 3 + 1
      d_idx <- triplet_start + (k - 1) * 3 + 2
      if (d_idx > n_cols) break
      candidate_col_pairs[[k]] <- list(name_idx = n_idx, votes_idx = d_idx)
    }

    for (kreis5 in unique(df_lk$kreis5)) {
      sub <- df_lk[df_lk$kreis5 == kreis5, ]

      # Sum eligible/voters/invalid/valid
      eligible <- sum(suppressWarnings(as.numeric(sub[[a_col]])), na.rm = TRUE)
      voters   <- sum(suppressWarnings(as.numeric(sub[[a_col + 1]])), na.rm = TRUE)
      invalid  <- sum(suppressWarnings(as.numeric(sub[[a_col + 2]])), na.rm = TRUE)
      valid    <- sum(suppressWarnings(as.numeric(sub[[a_col + 3]])), na.rm = TRUE)
      datum <- as.Date(as.character(sub[[2]][1]), format = "%d.%m.%Y")

      # Aggregate candidate votes. The N0k column in each Gemeinde row should
      # be the SAME candidate (the Kreis-wide candidates listed in identical
      # order). Use the first non-NA name from any row in this Kreis.
      cand_list <- list()
      for (cp in candidate_col_pairs) {
        names_in_kreis <- as.character(sub[[cp$name_idx]])
        names_clean <- names_in_kreis[!is.na(names_in_kreis) & names_in_kreis != ""]
        if (length(names_clean) == 0) next
        nm <- names_clean[1]
        votes <- sum(suppressWarnings(as.numeric(sub[[cp$votes_idx]])),
                     na.rm = TRUE)
        if (votes <= 0) next
        sp <- split_name_party(nm)
        cand_list[[length(cand_list) + 1]] <- tibble(
          candidate_name = sp$name,
          candidate_party = sp$party,
          candidate_votes = votes,
          candidate_voteshare = if (valid > 0) votes / valid else NA_real_
        )
      }
      if (length(cand_list) == 0) next

      cand_df <- bind_rows(cand_list) %>%
        mutate(
          ags = paste0(kreis5, "000"),
          ags_name = paste0("Landkreis ", kreis5),
          state = "15", state_name = "Sachsen-Anhalt",
          election_year = year_full,
          election_date = datum,
          election_type = "Landratswahl",
          round = round,
          eligible_voters = eligible,
          number_voters = voters,
          valid_votes = valid,
          invalid_votes = invalid,
          turnout = if (eligible > 0) voters / eligible else NA_real_
        )
      rows_out[[length(rows_out) + 1]] <- cand_df
    }
    if (length(rows_out) == 0) return(NULL)
    bind_rows(rows_out)
  }
  for (f in files) {
    yy <- str_match(basename(f), "lr(\\d{2})dat(\\d)\\.csv")[, 2:3]
    if (any(is.na(yy))) next
    year_full <- as.integer(yy[1]) + 2000L
    dat_n <- as.integer(yy[2])
    round <- if (dat_n %in% c(1L, 2L)) "stichwahl" else "hauptwahl"
    # Only Kreis-level files (dat3 for HW, dat1 for SW)
    if (!dat_n %in% c(1L, 3L)) next

    # Read CSV without headers, using positional column indexing.
    # 2007 schema: 1=ERGART, 2=DATUM, 3-5=LEER, 6=NR (8 digit), 7=NAME, 8-11=A/B/C/D, then triplets
    # 2014+ schema: 1=ERGART, 2=DATUM, 3-5=LEER, 6=Satzart, 7=Schlüsselnummer, 8=NAME, 9-12=A/B/C/D, then triplets
    raw <- iconv(readLines(f, warn = FALSE), from = "ISO-8859-1", to = "UTF-8")
    if (length(raw) < 2) next
    df <- read.csv2(text = raw[-1], stringsAsFactors = FALSE,
                    header = FALSE, check.names = FALSE)

    # Detect schema by checking column 6: if it's all numeric/NA → 2007 schema.
    # In 2014+ schema, col 6 has letters ("KRS", "GEM", "AS", etc.).
    col6_vals <- na.omit(unique(df[[6]]))
    is_2007 <- !any(grepl("[A-Za-z]", as.character(col6_vals)))

    if (is_2007) {
      # Aggregate Gemeinde rows to Kreis level
      kreis_df <- aggregate_st_gemeinden(df, year_full, round)
      if (!is.null(kreis_df)) out[[length(out) + 1]] <- kreis_df
      next  # skip the rest of the loop body for 2007
    }

    for (i in seq_len(nrow(df))) {
      r <- df[i, ]
      satzart <- as.character(r[[6]])
      if (is.na(satzart) || satzart != "KRS") next  # only Kreis-level rows
      raw_key <- as.character(r[[7]])
      if (is.na(raw_key) || raw_key == "") next
      schluessel <- str_pad(raw_key, width = 5, side = "left", pad = "0")
      if (!grepl("^[0-9]+$", schluessel) || nchar(schluessel) != 5) next

      kreis_name <- as.character(r[[8]])
      # Skip ST kreisfreie Städte (these are OB elections, not Landrat).
      # ST has 3: 15001 Dessau-Roßlau, 15002 Halle, 15003 Magdeburg.
      # Detect via name prefix or known AGS prefix.
      if (grepl("Kreisfreie|Krfr\\.|Landeshauptstadt|, Stadt$",
                kreis_name, ignore.case = TRUE)) next
      if (schluessel %in% c("15001", "15002", "15003")) next

      ags8 <- paste0(schluessel, "000")
      datum <- as.Date(as.character(r[[2]]), format = "%d.%m.%Y")
      eligible <- as.numeric(r[[9]])
      voters   <- as.numeric(r[[10]])
      invalid  <- as.numeric(r[[11]])
      valid    <- as.numeric(r[[12]])

      candidates <- list()
      n_cols <- ncol(df)
      # candidate triplets start at col 13: L01, N01, D01 → cols 13,14,15
      max_k <- min(11L, (n_cols - 12L) %/% 3L)
      for (k in seq_len(max_k)) {
        n_col_idx <- 13L + (k - 1L) * 3L + 1L  # 14, 17, 20, ...
        d_col_idx <- 13L + (k - 1L) * 3L + 2L  # 15, 18, 21, ...
        if (d_col_idx > n_cols) break
        nm_raw <- as.character(r[[n_col_idx]])
        if (is.na(nm_raw) || nm_raw == "") next
        sp <- split_name_party(nm_raw)
        candidates[[length(candidates) + 1]] <- tibble(
          candidate_name = sp$name,
          candidate_party = sp$party,
          candidate_votes = as.numeric(r[[d_col_idx]])
        )
      }
      if (length(candidates) == 0) next
      cand_df <- bind_rows(candidates) %>% filter(!is.na(candidate_votes))

      out[[length(out) + 1]] <- cand_df %>%
        mutate(
          ags = ags8, ags_name = kreis_name,
          state = "15", state_name = "Sachsen-Anhalt",
          election_year = year_full,
          election_date = datum,
          election_type = "Landratswahl",
          round = round,
          eligible_voters = eligible,
          number_voters = voters,
          valid_votes = valid,
          invalid_votes = invalid,
          # NB: `eligible` / `valid` are length-1 scalars, so ifelse() would
          # return a length-1 result and mutate() would RECYCLE the first
          # candidate's value onto every candidate in the election. Use a
          # scalar if/else on the scalar condition instead.
          turnout = if (!is.na(eligible) && eligible > 0) {
            voters / eligible
          } else NA_real_,
          candidate_voteshare = if (!is.na(valid) && valid > 0) {
            candidate_votes / valid
          } else NA_real_
        )
    }
  }
  if (length(out) == 0) return(NULL)
  st_all <- bind_rows(out)

  # lr07dat3.csv carries only Gemeinde rows, so the Kreis name has to be
  # synthesised and came out as the placeholder "Landkreis 15082". Fill in the
  # real names from the register so the 2007 rows are usable and match the way
  # the same Kreis is named in every other year.
  st_kreis_names <- c(
    "15081000" = "Altmarkkreis Salzwedel",
    "15082000" = "Landkreis Anhalt-Bitterfeld",
    "15083000" = "Landkreis Börde",
    "15084000" = "Burgenlandkreis",
    "15085000" = "Landkreis Harz",
    "15086000" = "Landkreis Jerichower Land",
    "15087000" = "Landkreis Mansfeld-Südharz",
    "15088000" = "Saalekreis",
    "15089000" = "Salzlandkreis",
    "15090000" = "Landkreis Stendal",
    "15091000" = "Landkreis Wittenberg"
  )
  placeholder <- grepl("^Landkreis 1[0-9]{4}$", st_all$ags_name)
  if (any(placeholder)) {
    fixed <- unname(st_kreis_names[st_all$ags[placeholder]])
    if (anyNA(fixed)) {
      stop("ST: no register name for ",
           paste(unique(st_all$ags[placeholder][is.na(fixed)]), collapse = ", "))
    }
    st_all$ags_name[placeholder] <- fixed
  }
  st_all
}

`%||%` <- function(a, b) {
  if (is.null(a)) return(b)
  if (is.data.frame(a) || is.list(a)) return(a)  # don't NA-check
  if (length(a) == 0) return(b)
  if (length(a) == 1 && is.na(a)) return(b)
  a
}

# ============================================================================
# Brandenburg parser (HTML pages from wahlen.brandenburg.de)
# ============================================================================
parse_bb <- function(raw_dir) {
  files <- list.files(raw_dir, pattern = "^BB_ergebnis.*\\.html$",
                       full.names = TRUE)
  if (length(files) == 0) return(NULL)

  bb_kreis_lookup <- tribble(
    ~slug,                         ~ags,        ~name,
    "barnim",                     "12060000",   "Landkreis Barnim",
    "dahme-spreewald",            "12061000",   "Landkreis Dahme-Spreewald",
    "elbe-elster",                "12062000",   "Landkreis Elbe-Elster",
    "havelland",                  "12063000",   "Landkreis Havelland",
    "maerkisch-oderland",         "12064000",   "Landkreis Märkisch-Oderland",
    "oberhavel",                  "12065000",   "Landkreis Oberhavel",
    "oberspreewald-lausitz",      "12066000",   "Landkreis Oberspreewald-Lausitz",
    "oder-spree",                 "12067000",   "Landkreis Oder-Spree",
    "ostprignitz-ruppin",         "12068000",   "Landkreis Ostprignitz-Ruppin",
    "potsdam-mittelmark",         "12069000",   "Landkreis Potsdam-Mittelmark",
    "prignitz",                   "12070000",   "Landkreis Prignitz",
    "spree-neisse",               "12071000",   "Landkreis Spree-Neiße",
    "teltow-flaeming",            "12072000",   "Landkreis Teltow-Fläming",
    "uckermark",                  "12073000",   "Landkreis Uckermark"
  )

  out <- list()
  for (f in files) {
    fname <- basename(f)
    is_sw <- grepl("stichwahl", fname)
    slug <- str_match(fname, "ergebnis-(?:landratswahl|stichwahl-landrat(?:-in)?)-([a-z-]+)\\.html")[, 2]
    info <- bb_kreis_lookup %>% filter(slug == !!slug)
    if (nrow(info) == 0) next

    pg <- read_html(f)

    # Try to find the date in the page text (e.g. "8. Oktober 2023")
    txt <- html_text(pg)
    date_match <- str_match(txt,
      "(\\d{1,2})\\.\\s*(Januar|Februar|März|April|Mai|Juni|Juli|August|September|Oktober|November|Dezember)\\s+(\\d{4})")
    if (any(is.na(date_match))) next
    months <- c("Januar"=1,"Februar"=2,"März"=3,"April"=4,"Mai"=5,"Juni"=6,
                "Juli"=7,"August"=8,"September"=9,"Oktober"=10,"November"=11,"Dezember"=12)
    election_date <- as.Date(sprintf("%s-%02d-%02d",
                                      date_match[, 4],
                                      months[date_match[, 3]],
                                      as.integer(date_match[, 2])))

    # Extract the result table (only one bb-table-stripes per page)
    tbl <- pg %>% html_element("table.bb-table-stripes")
    if (is.na(tbl)) next
    rows <- tbl %>% html_elements("tr") %>% html_elements("td") %>% html_text(trim = TRUE) %>%
      .[. != ""] %>% .[!. %in% c("Anzahl", "Prozent", "Merkmal")]

    rows_full <- tbl %>% html_elements("tr")
    parsed <- map_df(rows_full, function(tr) {
      tds <- tr %>% html_elements("td") %>% html_text(trim = TRUE)
      if (length(tds) < 3) return(tibble())
      tibble(merkmal = tds[1], anzahl = tds[2], prozent = tds[3])
    })

    if (nrow(parsed) == 0) next

    eligible <- parse_de_num(parsed$anzahl[grepl("Wahlberechtigte", parsed$merkmal)])[1]
    voters_raw <- parse_de_num(parsed$anzahl[grepl("W.hler", parsed$merkmal)])[1]
    invalid    <- parse_de_num(parsed$anzahl[grepl("Ung.ltige", parsed$merkmal)])[1]
    valid      <- parse_de_num(parsed$anzahl[grepl("G.ltige", parsed$merkmal)])[1]

    # Candidate rows: have a "(party)" suffix in merkmal
    cand_rows <- parsed %>%
      filter(grepl("\\(", merkmal),
             !grepl("Wahlberechtigte|W.hler|Ung.ltig|G.ltig", merkmal))
    if (nrow(cand_rows) == 0) next

    cands <- cand_rows %>%
      transmute(
        sp = map(merkmal, split_name_party),
        candidate_name  = map_chr(sp, "name"),
        candidate_party = map_chr(sp, "party"),
        candidate_votes = parse_de_num(anzahl),
        candidate_voteshare = parse_de_num(prozent) / 100
      ) %>% select(-sp)

    out[[length(out) + 1]] <- cands %>%
      mutate(
        ags = info$ags,
        ags_name = info$name,
        state = "12", state_name = "Brandenburg",
        election_year = year(election_date),
        election_date = election_date,
        election_type = "Landratswahl",
        round = if (is_sw) "stichwahl" else "hauptwahl",
        eligible_voters = eligible,
        number_voters = voters_raw,
        valid_votes = valid,
        invalid_votes = invalid,
        turnout = ifelse(!is.na(eligible) & eligible > 0, voters_raw / eligible, NA_real_)
      )
  }
  if (length(out) == 0) return(NULL)
  bind_rows(out)
}

# ============================================================================
# Sachsen parser (mixed: 2002 XLS, 2008 HTML, 2020/2022/2025 XLSX)
# ============================================================================
parse_sn <- function(raw_dir) {
  out <- list()

  # ---- 2008 + 2015 HTML pages (same wahlarchiv layout) ----
  html_files <- list.files(raw_dir,
                            pattern = "^LR_(2008|2015)_LR\\d{2}[12].*\\.html$",
                            full.names = TRUE)
  for (f in html_files) {
    fname <- basename(f)
    yr <- as.integer(str_match(fname, "LR_(\\d{4})_")[, 2])
    bzid <- str_match(fname, sprintf("LR%02d([12])", yr %% 100))[, 2]
    ort  <- str_match(fname, "_(\\d{5})\\.html$")[, 2]
    if (is.na(ort) || is.na(bzid)) next
    is_sw <- bzid == "2"

    pg <- read_html(f)
    txt <- html_text(pg)

    # Title contains Kreis name e.g. "...Landratswahl im Erzgebirgskreis"
    kreis_name <- str_squish(html_text(html_element(pg, "h2")))
    kreis_name <- str_match(kreis_name, "im\\s+(.+)$")[, 2] %||% "Unknown"

    # Date from H3 e.g. "Vorläufiges Ergebnis der Wahl am 08. Juni"
    h3 <- str_squish(html_text(html_element(pg, "h3")))
    date_match <- str_match(h3, "(\\d{1,2})\\.\\s*(\\w+)")
    months_de <- c("Januar"=1,"Februar"=2,"März"=3,"April"=4,"Mai"=5,"Juni"=6,
                   "Juli"=7,"August"=8,"September"=9,"Oktober"=10,"November"=11,"Dezember"=12)
    if (!is.na(date_match[1, 1]) && date_match[1, 3] %in% names(months_de)) {
      m <- months_de[date_match[1, 3]]
      d <- as.integer(date_match[1, 2])
      election_date <- as.Date(sprintf("%d-%02d-%02d", yr, m, d))
    } else {
      # Sensible defaults: 2008 HW = 2008-06-08, 2015 HW = 2015-06-07
      election_date <- as.Date(sprintf("%d-06-%02d", yr, if (yr == 2008) 8 else 7))
    }

    rows <- pg %>% html_elements("table tr")
    parsed <- map_df(rows, function(tr) {
      tds <- tr %>% html_elements("td") %>% html_text(trim = TRUE)
      if (length(tds) < 3) return(tibble())
      tibble(merkmal = str_squish(tds[1]),
             anzahl = parse_de_num(tds[2]),
             prozent = parse_de_num(tds[3]))
    })
    if (nrow(parsed) == 0) next

    # Locate the "Gewählter Landrat:" / "Gewählte Landrätin:" footer row.
    # Everything from there down is the duplicated winner row + Amtsvorgänger
    # block — exclude it.
    cutoff <- which(grepl("Gew.hlter? Landr|Gewählte Landrätin",
                          parsed$merkmal))
    if (length(cutoff) > 0) {
      parsed <- parsed[seq_len(cutoff[1] - 1), ]
    }

    # Use case-SENSITIVE matching for "Gültige Stimmen" — case-insensitive
    # would also match "Ungültige Stimmen" (lowercase "gültige" inside).
    eligible <- parsed$anzahl[grepl("Wahlberechtigte", parsed$merkmal, ignore.case = TRUE)][1]
    voters   <- parsed$anzahl[grepl("W.hler$", parsed$merkmal, ignore.case = TRUE)][1]
    invalid  <- parsed$anzahl[grepl("^Ung.ltige", parsed$merkmal, ignore.case = TRUE)][1]
    valid    <- parsed$anzahl[grepl("^G.ltige Stimmen$", parsed$merkmal, ignore.case = FALSE)][1]

    cands <- parsed %>%
      filter(grepl("\\([^)]+\\)$|^[A-ZÄÖÜ][a-zäöüß]+,", merkmal),
             !grepl("Wahlberechtigte|W.hler|Ung.ltig|^G.ltig|davon|Bewerber|^\\s*$",
                    merkmal, ignore.case = TRUE),
             !is.na(anzahl))

    if (nrow(cands) == 0) next

    cand_df <- cands %>%
      transmute(
        sp = map(merkmal, split_name_party),
        candidate_name  = map_chr(sp, "name"),
        candidate_party = map_chr(sp, "party"),
        candidate_votes = anzahl,
        candidate_voteshare = prozent / 100
      ) %>% select(-sp) %>%
      # Defensive dedup in case footer row slipped through
      distinct(candidate_name, .keep_all = TRUE)

    ags8 <- paste0(ort, "000")  # Sachsen 5-digit + 000
    out[[length(out) + 1]] <- cand_df %>%
      mutate(
        ags = ags8, ags_name = kreis_name,
        state = "14", state_name = "Sachsen",
        election_year = yr,
        election_date = election_date,
        election_type = "Landratswahl",
        round = if (is_sw) "stichwahl" else "hauptwahl",
        eligible_voters = eligible,
        number_voters = voters,
        valid_votes = valid,
        invalid_votes = invalid,
        turnout = ifelse(!is.na(eligible) & eligible > 0, voters / eligible, NA_real_)
      )
  }

  # ---- 2002 single XLS — winner-only (3 Landkreise) ----
  # Source has just 5 cols: Landkreis | Gewählt am (Excel serial) | Gewählter
  # Landrat | Wahlvorschlag | Gültige Stimmen %. We add winner-only rows
  # (no full candidate roster available from this file).
  f02 <- file.path(raw_dir, "LR_2002_gewaehlte_landraete.xls")
  if (file.exists(f02)) {
    d <- suppressMessages(read_excel(f02, sheet = 1, col_names = FALSE))
    # Map Sachsen Landkreis name → 5-digit Schlüssel (2002-era boundaries)
    sn_2002_lookup <- c(
      "Kamenz"        = "14292",
      "Meißen"        = "14280",
      "Vogtlandkreis" = "14178"
    )
    for (i in 5:nrow(d)) {
      kreis <- as.character(d[[1]][i])
      if (is.na(kreis) || !kreis %in% names(sn_2002_lookup)) next
      schluessel <- sn_2002_lookup[[kreis]]
      ags8 <- paste0(schluessel, "000")
      date_serial <- suppressWarnings(as.numeric(d[[2]][i]))
      election_date <- if (!is.na(date_serial)) {
        as.Date(date_serial, origin = "1899-12-30")
      } else NA
      winner <- as.character(d[[3]][i])
      party <- as.character(d[[4]][i])
      pct <- suppressWarnings(as.numeric(d[[5]][i]))
      out[[length(out) + 1]] <- tibble(
        candidate_name = winner,
        candidate_party = party,
        candidate_votes = NA_real_,  # not provided
        candidate_voteshare = pct / 100,
        ags = ags8, ags_name = paste0("Landkreis ", kreis),
        state = "14", state_name = "Sachsen",
        election_year = 2002,
        election_date = election_date,
        election_type = "Landratswahl",
        round = "hauptwahl",
        eligible_voters = NA_real_,
        number_voters = NA_real_,
        valid_votes = NA_real_,
        invalid_votes = NA_real_,
        turnout = NA_real_
      )
    }
  }

  # ---- 2022 Excel: aggregate Gemeinde rows to Kreis level ----
  # Each Kreis has its own sheet (e.g. "14521" = Erzgebirgskreis). The first
  # data row of each sheet is a Gemeinde, not a Kreis summary, so we aggregate.
  f22 <- file.path(raw_dir, "LR_2022_ergebnisse_endgueltig.xlsx")
  if (file.exists(f22)) {
    sheets_22 <- excel_sheets(f22)
    kreis_sheets <- sheets_22[grepl("^14[0-9]{3}$", sheets_22)]
    for (sh in kreis_sheets) {
      d <- suppressMessages(read_excel(f22, sheet = sh, col_names = FALSE))
      if (nrow(d) < 7) next

      # Title in row 2: "Endgültige Ergebnisse der Landratswahl ([2. Wahlgang])
      # am [date] in den Gemeinden des [KreisName] ([code])"
      title <- as.character(d[[1]][2])
      is_sw <- grepl("2. Wahlgang", title)
      date_match <- str_match(title,
        "(\\d{1,2}\\.\\s*\\w+\\s+\\d{4})")[, 2]
      months_de <- c("Januar"=1,"Februar"=2,"März"=3,"April"=4,"Mai"=5,"Juni"=6,
                     "Juli"=7,"August"=8,"September"=9,"Oktober"=10,
                     "November"=11,"Dezember"=12)
      election_date <- NA
      if (!is.na(date_match)) {
        parts <- str_match(date_match, "(\\d{1,2})\\.\\s*(\\w+)\\s+(\\d{4})")[1, ]
        if (parts[3] %in% names(months_de)) {
          election_date <- as.Date(sprintf("%s-%02d-%02d",
                                            parts[4],
                                            months_de[parts[3]],
                                            as.integer(parts[2])))
        }
      }
      kreis_name <- str_match(title, "des\\s+(.+?)\\s*\\(")[, 2]
      ags8 <- paste0(sh, "000")

      # Header rows 4-6: row 4 = parties, row 5 = candidate names,
      # row 6 = "absolut/in %" sub-headers. Cols 7+ are A1-A3, 7=Wahlb, 8=Wähler,
      # 12=ungültige, 14=gültige, then candidate triplets at 16, 18, 20, ...
      hdr_party <- as.character(d[4, ])
      hdr_name  <- as.character(d[5, ])
      hdr_kind  <- as.character(d[6, ])

      # Find candidate columns: where hdr_kind == "absolut" (German) and
      # the column has a name in row 5
      cand_col_idx <- which(grepl("absolut", hdr_kind, ignore.case = TRUE) &
                              !is.na(hdr_name))
      if (length(cand_col_idx) == 0) next

      # Aggregate Gemeinde rows (data rows start at row 7)
      data_rows <- d[7:nrow(d), ]
      eligible <- sum(suppressWarnings(as.numeric(data_rows[[7]])), na.rm = TRUE)
      voters   <- sum(suppressWarnings(as.numeric(data_rows[[8]])), na.rm = TRUE)
      invalid  <- sum(suppressWarnings(as.numeric(data_rows[[12]])), na.rm = TRUE)
      valid    <- sum(suppressWarnings(as.numeric(data_rows[[14]])), na.rm = TRUE)
      if (valid == 0 || nrow(data_rows) == 0) next

      candidates <- list()
      for (col_idx in cand_col_idx) {
        nm <- hdr_name[col_idx]
        pty <- hdr_party[col_idx]
        votes <- sum(suppressWarnings(as.numeric(data_rows[[col_idx]])),
                     na.rm = TRUE)
        if (votes <= 0) next
        candidates[[length(candidates) + 1]] <- tibble(
          candidate_name = str_squish(nm),
          candidate_party = str_squish(pty),
          candidate_votes = votes,
          candidate_voteshare = votes / valid
        )
      }
      if (length(candidates) == 0) next
      cand_df <- bind_rows(candidates) %>%
        mutate(
          ags = ags8, ags_name = kreis_name,
          state = "14", state_name = "Sachsen",
          election_year = 2022,
          election_date = election_date,
          election_type = "Landratswahl",
          round = if (is_sw) "stichwahl" else "hauptwahl",
          eligible_voters = eligible,
          number_voters = voters,
          valid_votes = valid,
          invalid_votes = invalid,
          turnout = ifelse(eligible > 0, voters / eligible, NA_real_)
        )
      out[[length(out) + 1]] <- cand_df
    }
  }

  # ---- 2020 Meißen + 2025 Mittelsachsen: single-Kreis Excel files ----
  # These have a Kreis-summary row in row 6 of sheet 1.
  for (single_file in c("LR_2020_LK_Meissen.xlsx", "LR_2025_LK_Mittelsachsen.xlsx")) {
    f <- file.path(raw_dir, single_file)
    if (!file.exists(f)) next

    d <- suppressMessages(read_excel(f, sheet = 1, col_names = FALSE))
    if (nrow(d) < 7) next

    title <- as.character(d[[1]][1])
    if (!grepl("Landratswahl", title, ignore.case = TRUE)) next
    date_match <- str_match(title,
      "(\\d{1,2})\\.\\s*(Januar|Februar|März|April|Mai|Juni|Juli|August|September|Oktober|November|Dezember)\\s+(\\d{4})")
    if (any(is.na(date_match))) next
    months_de <- c("Januar"=1,"Februar"=2,"März"=3,"April"=4,"Mai"=5,"Juni"=6,
                   "Juli"=7,"August"=8,"September"=9,"Oktober"=10,
                   "November"=11,"Dezember"=12)
    election_date <- as.Date(sprintf("%s-%02d-%02d",
                                      date_match[1, 4],
                                      months_de[date_match[1, 3]],
                                      as.integer(date_match[1, 2])))
    yr <- as.integer(date_match[1, 4])

    # Find rows: header rows 3-5, then data in row 6+
    # The Kreis-summary row is the one whose Schlüssel is 5-digit (not 8)
    sch_col <- as.character(d[[1]])
    kreis_row_idx <- which(grepl("^14[0-9]{3}$", sch_col))[1]
    if (is.na(kreis_row_idx)) next
    kreis_row <- d[kreis_row_idx, ]
    schluessel <- as.character(kreis_row[[1]])
    ags8 <- paste0(schluessel, "000")
    kreis_name <- as.character(kreis_row[[2]])

    eligible <- suppressWarnings(as.numeric(kreis_row[[3]]))
    voters   <- suppressWarnings(as.numeric(kreis_row[[4]]))
    invalid  <- suppressWarnings(as.numeric(kreis_row[[8]]))
    valid    <- suppressWarnings(as.numeric(kreis_row[[9]]))

    # Candidate header row: above kreis_row_idx, find row with names
    # (look for "(" indicating party in name)
    hdr_name <- character(0)
    for (try_row in (kreis_row_idx - 1):max(1, kreis_row_idx - 4)) {
      candidate_hdr <- as.character(d[try_row, ])
      if (sum(grepl("\\(", candidate_hdr), na.rm = TRUE) >= 1) {
        hdr_name <- candidate_hdr; break
      }
    }
    if (length(hdr_name) == 0) next

    # Candidate vote columns are even-indexed starting at 10
    cand_col_idx <- which(grepl("\\(", hdr_name) & seq_along(hdr_name) >= 10)
    if (length(cand_col_idx) == 0) next

    candidates <- list()
    for (col_idx in cand_col_idx) {
      sp <- split_name_party(hdr_name[col_idx])
      votes <- suppressWarnings(as.numeric(kreis_row[[col_idx]]))
      if (is.na(votes) || votes <= 0) next
      candidates[[length(candidates) + 1]] <- tibble(
        candidate_name = sp$name,
        candidate_party = sp$party,
        candidate_votes = votes,
        candidate_voteshare = ifelse(!is.na(valid) & valid > 0,
                                      votes / valid, NA_real_)
      )
    }
    if (length(candidates) == 0) next
    out[[length(out) + 1]] <- bind_rows(candidates) %>%
      mutate(
        ags = ags8, ags_name = kreis_name,
        state = "14", state_name = "Sachsen",
        election_year = yr,
        election_date = election_date,
        election_type = "Landratswahl",
        round = "hauptwahl",
        eligible_voters = eligible,
        number_voters = voters,
        valid_votes = valid,
        invalid_votes = invalid,
        turnout = ifelse(!is.na(eligible) & eligible > 0,
                         voters / eligible, NA_real_)
      )
  }

  if (length(out) == 0) return(NULL)
  bind_rows(out)
}

# ============================================================================
# Combine all
# ============================================================================
cat("=== Combining landrat data ===\n\n")

# ---- Thüringen pre-parsed data (from 00_th_parse.R) ----
parse_th_cached <- function() {
  f <- "data/landrat_elections/raw/thueringen_parsed.rds"
  if (!file.exists(f)) return(NULL)
  d <- readRDS(f)
  # Add the round column if not present (the parser stamps it but be safe)
  if (!"round" %in% names(d)) d$round <- "hauptwahl"
  d
}

# ---- Saarland extra Landrat data (from 00_sl_extra.R) ----
# This adds Landrat elections for the 5 SL Kreise that aren't in the
# RVS-only Wahldaten Excel. Most rows have only candidate_voteshare;
# absolute vote counts and aggregate stats are NA.
parse_sl_extra_cached <- function() {
  f <- "data/landrat_elections/raw/saarland_extra_parsed.rds"
  if (!file.exists(f)) return(NULL)
  readRDS(f)
}

# ---- Which rows does THIS script own? ----------------------------------------
# The scrapers/parsers above rebuild BB (12), SN (14), ST (15), TH (16) and the
# 5 extra SL Landkreise from source on every run. Everything else in the
# existing finals is written by the mayoral pipeline (BY/NRW/RLP/NI/HE/MV plus
# the Regionalverband Saarbrücken row of SL, AGS 10041000) and must be passed
# through untouched. NB: this AGS was 10000041 before the Saarland
# AGS-construction fix in 01_mayoral_unharm.R.
#
# The owned rows are DROPPED from the existing finals before the new rows are
# bound on. Without that this script is not idempotent: the unharm bind_rows()
# puts `existing` first inside distinct(), so a corrected re-parse could never
# update a row that was already there, and the candidate bind_rows() has no
# distinct() at all, so every consecutive run appended another full copy of the
# scraper states' candidate rows (527 per run, growing linearly).
scraper_states <- c("12", "14", "15", "16")
is_scraper_owned <- function(df) {
  st <- as.character(df$state)
  ag <- as.character(df$ags)
  # never NA: an NA state/ags counts as not-owned (matches the filter() this
  # replaced, which silently dropped NA rows from new_wide)
  (st %in% scraper_states) |
    (!is.na(st) & st == "10" & !is.na(ag) & ag != "10041000")
}

# Existing landrat data from mayoral pipeline
existing <- if (file.exists("data/landrat_elections/final/landrat_unharm.rds")) {
  readRDS("data/landrat_elections/final/landrat_unharm.rds")
} else NULL
# Defensive filter: drop any kreisfreie-Stadt rows that may have leaked from
# prior runs of this script (before the ST parser correctly excluded them).
if (!is.null(existing)) {
  existing <- existing %>% filter(
    !grepl("Kreisfreie|Krfr\\.|Landeshauptstadt", ags_name, ignore.case = TRUE)
  )
  drop_idx <- is_scraper_owned(existing)
  cat("Dropping", sum(drop_idx),
      "scraper-owned rows from existing landrat_unharm (rebuilt below)\n")
  existing_dropped <- existing[drop_idx, , drop = FALSE]  # for the check below
  existing <- existing[!drop_idx, , drop = FALSE]
}
cat("Existing landrat_unharm rows:", nrow(existing %||% data.frame(x = NA)), "\n")

st_data <- parse_st("data/landrat_elections/raw/sachsen_anhalt")
cat("ST parsed rows (candidate-level):", nrow(st_data %||% data.frame(x = NA)), "\n")

bb_data <- parse_bb("data/landrat_elections/raw/brandenburg")
cat("BB parsed rows (candidate-level):", nrow(bb_data %||% data.frame(x = NA)), "\n")

sn_data <- parse_sn("data/landrat_elections/raw/sachsen")
cat("SN parsed rows (candidate-level):", nrow(sn_data %||% data.frame(x = NA)), "\n")

th_data <- parse_th_cached()
cat("TH parsed rows (candidate-level):", nrow(th_data %||% data.frame(x = NA)), "\n")

sl_extra_data <- parse_sl_extra_cached()
cat("SL extra parsed rows (candidate-level):", nrow(sl_extra_data %||% data.frame(x = NA)), "\n")

# Build winner-level summaries (one row per ags+date+round) for landrat_unharm
build_unharm <- function(cand_df) {
  if (is.null(cand_df) || nrow(cand_df) == 0) return(NULL)
  cand_df %>%
    group_by(ags, ags_name, state, state_name, election_year, election_date,
             election_type, round) %>%
    summarise(
      eligible_voters = first(eligible_voters),
      number_voters = first(number_voters),
      valid_votes = first(valid_votes),
      invalid_votes = first(invalid_votes),
      turnout = first(turnout),
      # Winner: use candidate_votes when available, fall back to candidate_voteshare
      # for SN 2002 (winner-only file with no vote counts).
      winner_party = {
        if (any(!is.na(candidate_votes))) {
          candidate_party[which.max(candidate_votes)]
        } else if (any(!is.na(candidate_voteshare))) {
          candidate_party[which.max(candidate_voteshare)]
        } else NA_character_
      },
      winner_votes = if (any(!is.na(candidate_votes))) {
        max(candidate_votes, na.rm = TRUE)
      } else NA_real_,
      # Winner share: compute it from the winner's OWN votes whenever the vote
      # counts are available. max(candidate_voteshare) is only a valid
      # winner-share for share-only sources (SN 2002, SL Wikipedia rows) — where
      # per-candidate shares are damaged (SL Neunkirchen 2024's 0.09) or were
      # recycled, max() returns some other candidate's number and publishes it
      # as the winner's.
      winner_voteshare = if (!is.na(first(valid_votes)) && first(valid_votes) > 0 &&
                             !is.na(winner_votes)) {
        winner_votes / first(valid_votes)
      } else if (any(!is.na(candidate_voteshare))) {
        max(candidate_voteshare, na.rm = TRUE)
      } else NA_real_,
      .groups = "drop"
    )
}

st_unharm <- build_unharm(st_data)
bb_unharm <- build_unharm(bb_data)
sn_unharm <- build_unharm(sn_data)
th_unharm <- build_unharm(th_data)
sl_extra_unharm <- build_unharm(sl_extra_data)

cat("\nUnharm rows added:\n")
cat("  ST:", nrow(st_unharm %||% data.frame(x = NA)), "\n")
cat("  BB:", nrow(bb_unharm %||% data.frame(x = NA)), "\n")
cat("  SN:", nrow(sn_unharm %||% data.frame(x = NA)), "\n")
cat("  TH:", nrow(th_unharm %||% data.frame(x = NA)), "\n")
cat("  SL (extra Kreise):", nrow(sl_extra_unharm %||% data.frame(x = NA)), "\n")

# Combine
new_unharm <- bind_rows(st_unharm, bb_unharm, sn_unharm, th_unharm, sl_extra_unharm)

# Safety net for the destructive drop above: a state whose rows were removed
# from the existing file MUST come back from its own parser. If a 00_* scraper
# was never run (or its raw dir is missing) the parser returns NULL silently,
# and without this check the drop would quietly delete that state's data.
if (exists("existing_dropped") && nrow(existing_dropped) > 0) {
  lost <- setdiff(unique(as.character(existing_dropped$state)),
                  unique(as.character(new_unharm$state)))
  if (length(lost) > 0) {
    stop("Scraper-owned rows were dropped from landrat_unharm but NOT rebuilt ",
         "for state(s): ", paste(lost, collapse = ", "),
         ". Run the corresponding 00_*_scrape.R / 00_*_parse.R first.")
  }
}

combined_unharm <- bind_rows(existing, new_unharm) %>%
  distinct(ags, election_date, election_type, round, .keep_all = TRUE) %>%
  arrange(state, ags, election_year, election_date, round)

cat("\nFinal landrat_unharm rows:", nrow(combined_unharm), "\n")
cat("By state:\n")
print(combined_unharm %>% count(state, state_name))

# Save unharm
write_rds(combined_unharm, "data/landrat_elections/final/landrat_unharm.rds")
fwrite(combined_unharm, "data/landrat_elections/final/landrat_unharm.csv")
cat("\n✓ Saved landrat_unharm.{rds,csv}\n")

# ============================================================================
# Build candidate-level wide format
# ============================================================================
# Merge HW + SW per (ags, candidate_name) for the new scraped states (ST/BB/SN/TH)
# and concat with the existing landrat_candidates (from mayoral pipeline).

cat("\n=== Building landrat_candidates wide format ===\n")

# Combine all new long-format data
new_long <- bind_rows(st_data, bb_data, sn_data, th_data, sl_extra_data)
cat("New long-format rows:", nrow(new_long), "\n")

if (nrow(new_long) > 0) {
  # Compute candidate_rank and n_candidates per (ags, election_date, round).
  # When candidate_votes is NA across the group (e.g. SN 2002 winner-only XLS,
  # SL Wikipedia hardcoded rows with %s only), fall back to candidate_voteshare
  # so we still get a valid rank-1 winner.
  new_long <- new_long %>%
    group_by(ags, election_date, election_type, round) %>%
    mutate(
      candidate_rank = if (any(!is.na(candidate_votes))) {
        rank(-candidate_votes, ties.method = "min", na.last = "keep")
      } else if (any(!is.na(candidate_voteshare))) {
        rank(-candidate_voteshare, ties.method = "min", na.last = "keep")
      } else {
        rep(NA_integer_, n())
      },
      n_candidates = n(),
      is_winner = !is.na(candidate_rank) & candidate_rank == 1
    ) %>%
    ungroup()

  # Split HW and SW
  hw <- new_long %>% filter(round == "hauptwahl")
  sw <- new_long %>% filter(round == "stichwahl")

  cat("  HW candidate rows:", nrow(hw), "\n")
  cat("  SW candidate rows:", nrow(sw), "\n")

  # Find HW-SW date pairs (per ags, SW within 60 days after HW)
  date_pairs <- sw %>%
    distinct(ags, sw_date = election_date) %>%
    inner_join(hw %>% distinct(ags, hw_date = election_date),
               by = "ags", relationship = "many-to-many") %>%
    mutate(gap = as.numeric(sw_date - hw_date)) %>%
    filter(gap > 0, gap < 60) %>%
    group_by(ags, sw_date) %>%
    slice_min(gap, n = 1, with_ties = FALSE) %>%
    ungroup() %>% select(ags, hw_date, sw_date)
  cat("  Election cycles with Stichwahl:", nrow(date_pairs), "\n")

  # Tag SW rows with their matching HW date
  sw_keyed <- sw %>%
    inner_join(date_pairs, by = c("ags", "election_date" = "sw_date")) %>%
    transmute(
      ags, hw_date, candidate_name,
      election_date_sw = election_date,
      candidate_votes_sw = candidate_votes,
      candidate_voteshare_sw = candidate_voteshare,
      candidate_rank_sw = candidate_rank,
      is_winner_sw = is_winner,
      n_candidates_sw = n_candidates
    ) %>%
    distinct(ags, hw_date, candidate_name, .keep_all = TRUE)

  # Build wide HW + SW
  hw_wide <- hw %>%
    left_join(date_pairs %>% select(ags, hw_date, election_date_sw = sw_date),
              by = c("ags", "election_date" = "hw_date")) %>%
    left_join(sw_keyed,
              by = c("ags", "election_date" = "hw_date", "candidate_name")) %>%
    mutate(
      election_date_sw = coalesce(election_date_sw.x, election_date_sw.y),
      has_stichwahl = !is.na(election_date_sw)
    ) %>%
    select(-election_date_sw.x, -election_date_sw.y) %>%
    rename(
      candidate_votes_hw = candidate_votes,
      candidate_voteshare_hw = candidate_voteshare,
      candidate_rank_hw = candidate_rank,
      n_candidates_hw = n_candidates
    ) %>%
    mutate(
      is_winner = case_when(
        has_stichwahl & !is.na(is_winner_sw) ~ is_winner_sw,
        TRUE ~ is_winner
      )
    ) %>%
    select(-is_winner_sw, -round)

  # SW-only candidates (joined runoff but didn't run in HW)
  sw_only <- sw %>%
    inner_join(date_pairs, by = c("ags", "election_date" = "sw_date")) %>%
    anti_join(hw,
              by = c("ags", "hw_date" = "election_date", "candidate_name")) %>%
    mutate(
      election_date_sw = election_date,
      election_date = hw_date,
      has_stichwahl = TRUE,
      candidate_votes_sw = candidate_votes,
      candidate_voteshare_sw = candidate_voteshare,
      candidate_rank_sw = candidate_rank,
      n_candidates_sw = n_candidates,
      candidate_votes_hw = NA_real_,
      candidate_voteshare_hw = NA_real_,
      candidate_rank_hw = NA_integer_,
      n_candidates_hw = NA_integer_
    ) %>%
    select(-round, -hw_date,
           -candidate_votes, -candidate_voteshare, -candidate_rank, -n_candidates)

  new_wide <- bind_rows(hw_wide, sw_only)

  # Orphaned SW rows: a Stichwahl with NO Hauptwahl partner in the raw grab
  # (SN 2022's six runoff Kreise, whose 12 June first round was never
  # downloaded, and three SL Stichwahlen). date_pairs only holds SW dates that
  # have an HW within 60 days, and both sw_keyed and sw_only inner_join it — so
  # without this branch these candidates vanish from landrat_candidates
  # entirely, even though build_unharm keeps the election in landrat_unharm.
  # Mirrors Step 6 of code/mayoral_elections/01b_mayoral_candidates.R: the SW
  # date stays the primary election_date and the HW columns are NA.
  sw_orphaned <- sw %>%
    anti_join(date_pairs, by = c("ags", "election_date" = "sw_date"))

  # ... except a Stichwahl dated the SAME DAY as a Hauptwahl of the same Kreis.
  # That is never a real runoff-without-first-round; it only happens when
  # 00_th_parse.R could not read a date and fell back to <year>-01-01 for BOTH
  # rounds (TH Saale-Orla-Kreis 16075000 in 2006/2012 — those sheets carry no
  # "Stand:"/"erstellt am:" timestamp). The zero-day gap keeps the pair out of
  # date_pairs, and emitting the SW as an orphan would produce wide rows that
  # collide with the Hauptwahl's own rows on (ags, election_date,
  # candidate_name). Keep them out, but say so.
  hw_dates <- hw %>% distinct(ags, election_date)
  same_day <- sw_orphaned %>% semi_join(hw_dates, by = c("ags", "election_date"))
  if (nrow(same_day) > 0) {
    cat("  NOTE: dropping", nrow(same_day),
        "SW rows dated the same day as their own Hauptwahl (unreadable source",
        "date):", paste(unique(paste0(same_day$ags, " ", same_day$election_date)),
                        collapse = ", "), "\n")
    sw_orphaned <- sw_orphaned %>% anti_join(hw_dates, by = c("ags", "election_date"))
  }

  if (nrow(sw_orphaned) > 0) {
    cat("  WARNING: Orphaned SW rows (no matching HW date):", nrow(sw_orphaned), "\n")
    orphaned_wide <- sw_orphaned %>%
      mutate(
        election_date_sw = election_date,
        has_stichwahl = TRUE,
        candidate_votes_sw = candidate_votes,
        candidate_voteshare_sw = candidate_voteshare,
        candidate_rank_sw = candidate_rank,
        n_candidates_sw = n_candidates,
        candidate_votes_hw = NA_real_,
        candidate_voteshare_hw = NA_real_,
        candidate_rank_hw = NA_integer_,
        n_candidates_hw = NA_integer_
      ) %>%
      select(-round,
             -candidate_votes, -candidate_voteshare, -candidate_rank, -n_candidates)
    new_wide <- bind_rows(new_wide, orphaned_wide)
  } else {
    cat("  No orphaned SW rows\n")
  }

  # Add columns to match existing landrat_candidates schema (32 cols)
  new_wide <- new_wide %>%
    mutate(
      candidate_last_name = str_trim(str_extract(candidate_name, "^[^,]+")),
      candidate_first_name = str_trim(str_extract(candidate_name, ",\\s*(.+)$", group = 1)),
      candidate_gender = NA_character_,
      turnout_sw = NA_real_,
      candidate_birth_year = NA_real_,
      candidate_profession = NA_character_,
      office_type = NA_character_
    )

  # Read existing candidates from mayoral pipeline
  existing_cands <- if (file.exists("data/landrat_elections/final/landrat_candidates.rds")) {
    readRDS("data/landrat_elections/final/landrat_candidates.rds")
  } else NULL

  # Defensive filter for kreisfreie-Stadt leaks
  if (!is.null(existing_cands)) {
    existing_cands <- existing_cands %>% filter(
      !grepl("Kreisfreie|Krfr\\.|Landeshauptstadt", ags_name, ignore.case = TRUE)
    )
    # Idempotency: drop the rows this script rebuilds (see is_scraper_owned()
    # above). The bind_rows() below has no distinct(), so without this a second
    # consecutive run appends a byte-identical copy of every scraper-state
    # candidate row, and every further run adds another.
    drop_idx <- is_scraper_owned(existing_cands)
    cat("Dropping", sum(drop_idx),
        "scraper-owned rows from existing landrat_candidates (rebuilt below)\n")
    existing_cands <- existing_cands[!drop_idx, , drop = FALSE]
  }

  # Align column sets
  cols_existing <- names(existing_cands)
  for (c in cols_existing) {
    if (!c %in% names(new_wide)) new_wide[[c]] <- NA
  }
  new_wide <- new_wide[, cols_existing]

  # Filter new_wide to the states this script owns, to avoid cross-pipeline
  # duplication: the mayoral pipeline owns BY/NRW/RLP/NI/HE/MV for candidates
  # and the Regionalverband Saarbrücken row of SL. Same predicate that removed
  # those rows from existing_cands above — the two MUST stay in lockstep.
  new_wide <- new_wide[is_scraper_owned(new_wide), , drop = FALSE]

  combined_cands <- bind_rows(existing_cands, new_wide) %>%
    arrange(state, ags, election_year, election_date)

  # Re-apply the Sachsen-Anhalt anonymisation (see helper above). 01b applies it
  # to landrat_candidates, but this script rewrites that file, so it has to be
  # re-applied to the newly bound rows or the anonymisation is order-dependent.
  cat("\n=== Anonymising Sachsen-Anhalt losing candidates (StaLA licence) ===\n")
  combined_cands <- anonymise_st_losers(combined_cands, "landrat_candidates")

  cat("Final landrat_candidates rows:", nrow(combined_cands), "\n")
  cat("By state:\n")
  print(combined_cands %>% count(state, state_name))

  write_rds(combined_cands, "data/landrat_elections/final/landrat_candidates.rds")
  fwrite(combined_cands, "data/landrat_elections/final/landrat_candidates.csv")
  cat("\n✓ Saved landrat_candidates.{rds,csv}\n")
}
