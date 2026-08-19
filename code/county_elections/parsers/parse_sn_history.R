# Parse the Saxon county-council reform-wave elections of 1994 and 1995.
#
# The statistical office distributed these tables as early BIFF .XLS files
# that libxls/readxl cannot open directly. The source files are never changed:
# each required workbook is converted to a temporary .xlsx with LibreOffice.
#
# The 1994 publication reports exact eligible-voter, voter, and party-vote
# counts by county, but only rounded percentages for valid/invalid ballots.
# Accordingly, valid_votes and invalid_votes are NA for 1994 rather than
# pretending that reconstructed counts are exact. Two 1994 elections
# (Elstertal and Göltzschtal) were annulled when the proposed counties were
# declared invalid, and are deliberately not returned.
#
# The 1995 publication contains full elections for three newly formed counties
# and narrowly scoped supplementary elections in Uhyst and Schönfeld-Weißig.
# The latter two rows therefore retain municipality AGS values while
# contest_type correctly records that the elected body was a Kreistag.

set.seed(20260730)

.sn_require_packages <- function() {
  required <- c("readxl", "dplyr", "tibble")
  missing <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing) > 0L) {
    stop("Missing required R package(s): ", paste(missing, collapse = ", "),
         call. = FALSE)
  }
}

.sn_as_number <- function(x) {
  x <- trimws(as.character(x))
  x[x %in% c("", "-", "x", "x   ", "x     ", "x      ")] <- NA_character_
  suppressWarnings(as.numeric(x))
}

.sn_find_soffice <- function() {
  candidate <- Sys.which("soffice")
  if (nzchar(candidate)) return(unname(candidate))

  bundled <- file.path(
    path.expand("~"),
    ".cache/codex-runtimes/codex-primary-runtime/dependencies/bin/override/soffice"
  )
  if (file.exists(bundled)) return(bundled)

  stop(
    "LibreOffice/soffice is required to read the Saxony 1994/1995 BIFF files.",
    call. = FALSE
  )
}

.sn_read_biff <- function(path) {
  if (!file.exists(path)) stop("Missing Saxony source file: ", path, call. = FALSE)

  out_dir <- tempfile("sn_biff_")
  profile_dir <- tempfile("sn_lo_profile_")
  dir.create(out_dir)
  dir.create(profile_dir)
  on.exit(unlink(c(out_dir, profile_dir), recursive = TRUE, force = TRUE), add = TRUE)

  profile_uri <- paste0("file://", normalizePath(profile_dir, winslash = "/"))
  output <- system2(
    .sn_find_soffice(),
    args = c(
      paste0("-env:UserInstallation=", profile_uri),
      "--headless", "--convert-to", "xlsx",
      "--outdir", shQuote(out_dir), shQuote(normalizePath(path))
    ),
    stdout = TRUE,
    stderr = TRUE
  )
  status <- attr(output, "status")
  if (!is.null(status) && status != 0L) {
    stop(
      "LibreOffice failed to convert ", basename(path), ":\n",
      paste(output, collapse = "\n"),
      call. = FALSE
    )
  }

  converted <- file.path(
    out_dir,
    paste0(tools::file_path_sans_ext(basename(path)), ".xlsx")
  )
  if (!file.exists(converted) || file.info(converted)$size == 0) {
    stop(
      "LibreOffice did not create a non-empty conversion for ", basename(path),
      ". Output:\n", paste(output, collapse = "\n"),
      call. = FALSE
    )
  }

  suppressMessages(
    as.data.frame(
      readxl::read_excel(converted, col_names = FALSE, col_types = "text"),
      stringsAsFactors = FALSE
    )
  )
}

.sn_key <- function(x) {
  x <- iconv(as.character(x), from = "", to = "ASCII//TRANSLIT")
  x <- tolower(x)
  x <- gsub("[0-9]+\\)", "", x)
  x <- gsub("\\blandkreis\\b", "", x)
  gsub("[^a-z0-9]", "", x)
}

.sn_1994_county_codes <- c(
  annaberg = "14171",
  bautzen = "14272",
  chemnitzerland = "14173",
  delitzsch = "14374",
  dobeln = "14375",
  freiberg = "14177",
  leipzigerland = "14379",
  mittlerererzgebirgskreis = "14181",
  mittweida = "14182",
  muldentalkreis = "14383",
  niederschlesischeroberlausitzkreis = "14284",
  riesagrossenhain = "14285",
  lobauzittau = "14286",
  sachsischeschweiz = "14287",
  stollberg = "14188",
  torgauoschatz = "14389",
  weisseritzkreis = "14290",
  aueschwarzenberg = "14191",
  zwickauerland = "14193"
)

.sn_canonical_1994_name <- function(previous, current) {
  previous <- trimws(as.character(previous))
  current <- trimws(as.character(current))
  if (is.na(previous)) previous <- ""
  if (is.na(current)) current <- ""
  name <- if (nzchar(current)) paste(previous, current) else previous
  name <- gsub("[0-9]+\\)", "", name)
  name <- gsub("-\\s+kreis$", "kreis", name, ignore.case = TRUE)
  name <- trimws(gsub("\\s+", " ", name))
  key <- .sn_key(name)

  if (startsWith(key, "mittlerererzgebirgs")) {
    return("Mittlerer Erzgebirgskreis")
  }
  if (startsWith(key, "niederschlesischeroberlausitz")) {
    return("Niederschlesischer Oberlausitzkreis")
  }
  name
}

.sn_parse_1994 <- function(sn_dir) {
  turnout_raw <- .sn_read_biff(file.path(
    sn_dir, "Sachsen_1994_Kreistagswahl", "KT94_SN_03.XLS"
  ))
  votes_raw <- .sn_read_biff(file.path(
    sn_dir, "Sachsen_1994_Kreistagswahl", "KT94_SN_05.XLS"
  ))

  turnout <- lapply(seq_len(nrow(turnout_raw)), function(i) {
    eligible <- .sn_as_number(turnout_raw[i, 2])
    voters <- .sn_as_number(turnout_raw[i, 4])
    if (is.na(eligible) || is.na(voters)) return(NULL)
    name <- trimws(turnout_raw[i, 1])
    key <- .sn_key(name)
    if (!key %in% names(.sn_1994_county_codes)) return(NULL)
    data.frame(
      key = key,
      eligible_voters = eligible,
      number_voters = voters,
      stringsAsFactors = FALSE
    )
  })
  turnout <- dplyr::bind_rows(turnout)

  party_names <- c(
    "cdu", "spd", "pds", "gruene", "fdp", "rep", "dsu",
    "andere_parteien", "waehlervereinigungen"
  )
  vote_rows <- lapply(seq_len(nrow(votes_raw)), function(i) {
    row_type <- tolower(trimws(as.character(votes_raw[i, 2])))
    if (is.na(row_type) || row_type != "absolut") return(NULL)
    total <- .sn_as_number(votes_raw[i, 3])
    if (is.na(total)) return(NULL)

    name <- .sn_canonical_1994_name(
      if (i > 1L) votes_raw[i - 1L, 1] else "",
      votes_raw[i, 1]
    )
    key <- .sn_key(name)
    if (!key %in% names(.sn_1994_county_codes)) return(NULL)

    party_counts <- .sn_as_number(unlist(votes_raw[i, 4:12], use.names = FALSE))
    party_counts[is.na(party_counts)] <- 0
    if (sum(party_counts) != total) {
      stop(
        "1994 party counts do not sum to valid votes for ", name,
        ": ", sum(party_counts), " != ", total,
        call. = FALSE
      )
    }

    row <- data.frame(
      key = key,
      ags_name = name,
      gueltige_stimmen = total,
      stringsAsFactors = FALSE
    )
    row[party_names] <- as.list(party_counts)
    row
  })
  votes <- dplyr::bind_rows(vote_rows)

  if (nrow(turnout) != length(.sn_1994_county_codes) ||
      nrow(votes) != length(.sn_1994_county_codes)) {
    stop(
      "Expected ", length(.sn_1994_county_codes),
      " valid 1994 county elections, found ", nrow(turnout),
      " turnout rows and ", nrow(votes), " vote rows.",
      call. = FALSE
    )
  }
  if (anyDuplicated(turnout$key) || anyDuplicated(votes$key)) {
    stop("Duplicate 1994 Saxony county keys in source tables.", call. = FALSE)
  }

  out <- dplyr::inner_join(votes, turnout, by = "key")
  if (nrow(out) != length(.sn_1994_county_codes)) {
    stop("1994 Saxony turnout/vote join lost county rows.", call. = FALSE)
  }

  out$ags <- unname(.sn_1994_county_codes[out$key])
  out$county <- out$ags
  out$state <- "14"
  out$election_year <- 1994L
  out$valid_votes <- NA_real_
  out$invalid_votes <- NA_real_
  out$turnout <- out$number_voters / out$eligible_voters
  out$result_level <- "county"
  out$contest_type <- "kreistag"
  out$event_scope <- "split_reform"

  for (party in party_names) {
    out[[party]] <- out[[party]] / out$gueltige_stimmen
  }
  out$gueltige_stimmen <- NULL
  out$key <- NULL

  out[, c(
    "ags", "ags_name", "county", "state", "election_year",
    "eligible_voters", "number_voters", "valid_votes", "invalid_votes",
    "turnout", "result_level", "contest_type", "event_scope", party_names
  )]
}

.sn_party_key_1995 <- function(x) {
  key <- .sn_key(x)
  switch(
    key,
    cdu = "cdu",
    spd = "spd",
    pds = "pds",
    grune = "gruene",
    grunen = "gruene",
    fdp = "fdp",
    dsu = "dsu",
    rep = "rep",
    forum = "forum",
    wahlervereinigungen = "waehlervereinigungen",
    NA_character_
  )
}

.sn_parse_1995 <- function(sn_dir) {
  raw <- .sn_read_biff(file.path(
    sn_dir, "Sachsen_1995_Kreistagswahl", "KT95_TAB1.XLS"
  ))

  events <- data.frame(
    source_heading = c(
      "Vogtlandkreis", "Meißen-Radebeul", "Westlausitz-Dresdner Land",
      "Uhyst", "Schönfeld-Weißig"
    ),
    ags = c("14178", "14280", "14292", "14284420", "14287360"),
    ags_name = c(
      "Vogtlandkreis", "Meißen-Radebeul", "Westlausitz-Dresdner Land",
      "Uhyst", "Schönfeld-Weißig"
    ),
    result_level = c("county", "county", "county", "municipality", "municipality"),
    stringsAsFactors = FALSE
  )

  col2_keys <- .sn_key(raw[[2]])
  starts <- vapply(
    events$source_heading,
    function(heading) {
      hit <- which(col2_keys == .sn_key(heading))
      if (length(hit) != 1L) {
        stop("Expected one 1995 heading for ", heading, ", found ", length(hit),
             call. = FALSE)
      }
      hit
    },
    integer(1)
  )

  party_names <- c(
    "cdu", "spd", "pds", "gruene", "fdp", "dsu", "rep", "forum",
    "waehlervereinigungen"
  )
  parsed <- lapply(seq_len(nrow(events)), function(j) {
    start <- starts[j]
    end <- if (j < nrow(events)) starts[j + 1L] - 1L else nrow(raw)
    block <- raw[start:end, , drop = FALSE]
    metric_keys <- .sn_key(block[[1]])

    metric <- function(label) {
      hit <- which(metric_keys == .sn_key(label))
      if (length(hit) != 1L) {
        stop(
          "Expected one ", label, " row in 1995 block ",
          events$source_heading[j], ", found ", length(hit),
          call. = FALSE
        )
      }
      value <- .sn_as_number(block[hit, 2])
      if (is.na(value)) {
        stop("Missing 1995 ", label, " count for ", events$source_heading[j],
             call. = FALSE)
      }
      value
    }

    eligible <- metric("Wahlberechtigte")
    voters <- metric("Wähler")
    invalid <- metric("Ungültige Stimmzettel")
    valid <- metric("Gültige Stimmzettel")
    total <- metric("Gültige Stimmen")

    if (valid + invalid != voters) {
      stop("1995 valid and invalid ballots do not equal voters for ",
           events$source_heading[j], call. = FALSE)
    }

    party_counts <- stats::setNames(rep(0, length(party_names)), party_names)
    for (i in seq_len(nrow(block))) {
      party <- .sn_party_key_1995(block[i, 1])
      if (is.na(party)) next
      count <- .sn_as_number(block[i, 2])
      if (!is.na(count)) party_counts[party] <- count
    }
    if (sum(party_counts) != total) {
      stop(
        "1995 party counts do not sum to valid votes for ",
        events$source_heading[j], ": ", sum(party_counts), " != ", total,
        call. = FALSE
      )
    }

    row <- data.frame(
      ags = events$ags[j],
      ags_name = events$ags_name[j],
      county = substr(events$ags[j], 1L, 5L),
      state = "14",
      election_year = 1995L,
      eligible_voters = eligible,
      number_voters = voters,
      valid_votes = valid,
      invalid_votes = invalid,
      turnout = voters / eligible,
      result_level = events$result_level[j],
      contest_type = "kreistag",
      event_scope = "split_reform",
      stringsAsFactors = FALSE
    )
    row[party_names] <- as.list(party_counts / total)
    row
  })

  dplyr::bind_rows(parsed)
}

#' Parse Saxony's 1994/1995 county-election reform wave
#'
#' @param raw_dir Either the state directory ending in `Sachsen` or the parent
#'   directory containing a `Sachsen` subdirectory.
#' @return A tibble in the current unharmonized county-election vote schema,
#'   with additive result-level, contest-type, and event-scope metadata.
parse_sn_historical_county_elections <- function(raw_dir) {
  .sn_require_packages()
  if (!dir.exists(raw_dir)) stop("Raw directory does not exist: ", raw_dir, call. = FALSE)

  sn_dir <- if (basename(normalizePath(raw_dir)) == "Sachsen") {
    normalizePath(raw_dir)
  } else {
    file.path(normalizePath(raw_dir), "Sachsen")
  }
  if (!dir.exists(sn_dir)) {
    stop("Could not find Saxony raw directory under: ", raw_dir, call. = FALSE)
  }

  out <- dplyr::bind_rows(.sn_parse_1994(sn_dir), .sn_parse_1995(sn_dir))
  out$ags <- ifelse(
    out$result_level == "county" & nchar(out$ags) == 5L,
    paste0(out$ags, "000"),
    out$ags
  )
  if (anyDuplicated(paste(out$ags, out$election_year, sep = "_"))) {
    stop("Duplicate Saxony historical AGS x election-year rows.", call. = FALSE)
  }
  if (!identical(sort(unique(out$election_year)), c(1994L, 1995L))) {
    stop("Saxony historical parser did not return exactly 1994 and 1995.",
         call. = FALSE)
  }
  tibble::as_tibble(out)
}
