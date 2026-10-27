# Party family of a mayoral / Landrat Wahlvorschlag label
#
# The sources print the Wahlvorschlag as free text: "CSU", "CSU/Freie
# Wählergemeinschaft", "Bündnis 90/Die Grünen", "kein Wahlvorschlag", the name
# of a local list, or (Rheinland-Pfalz) an independent candidate's own surname.
# 14,700 distinct labels. party_switch and is_new_party_mayor compared the
# labels themselves, so a re-elected mayor whose list was spelled differently,
# or two successive local lists, counted as a party switch (audit 2026-10,
# M22; user decision 2026-10-19: compare party families, keep the labels).
#
# Families (codes as in GERDA's council and state files where they mean the
# same thing):
#   cdu_csu, spd, gruene, linke_pds, fdp, afd  the parties, incl. predecessors
#                       (PDS, Bündnis 90, GAL, FDP/DVP)
#   fw_wg               FREIE WÄHLER or any local Wählergruppe / Bürgerliste.
#                       NOT the council files' freie_wahler, which is the
#                       FREIE WÄHLER party alone: a mayoral label cannot tell
#                       the party from a local "Freie Wähler" list.
#   independent         Einzelbewerber, parteilos, kein Wahlvorschlag, a
#                       candidate's own name; Sachsen-Anhalt's blank label
#   other               another named party (BP, ÖDP, NPD, Die PARTEI, ...)
#   joint_list          a list of two or more of the families above
#                       ("CSU/Freie Wählergemeinschaft", "SPD/GRÜNE")
#   NA                  no label (Baden-Württemberg publishes none), or a
#                       source category that mixes families
#
# party_family_parts() returns the components of a label as one string,
# "cdu_csu|fw_wg"; a minor party is "other:<party>" so that BP -> ÖDP is a
# change. party_switch is 0 when the components of two successive winners
# overlap (CSU -> CSU/FW), 1 when they do not, NA when either is unknown.
#
# Used by 01b_mayoral_candidates.R, 03_mayor_panel.R and
# code/landrat_elections/01_landrat_combine.R; 99_audit.R §36 pins the
# classification of the most frequent labels.

# Labels pinned whole (compared after pf_norm()).
pf_pinned <- c(
  # Nordrhein-Westfalen's category for "a Wählergruppe or an Einzelbewerber":
  # the two families cannot be told apart.
  "wählergr/einzelbew" = NA_character_,
  "wählergruppe/einzelbewerber" = NA_character_,
  # Sachsen: "sonstige Einzelvorschläge"
  "sonstige ev" = "independent",
  "ev" = "independent",
  "parteios" = "independent",                          # misprint, Bayern
  "partei- u listenfreier bürgermeisterkandidat" = "independent",
  # a joint list whose members the source does not name
  "gemeinsamer wahlvorschlag" = "joint_list",
  "gemeinsame liste" = "joint_list",
  "gemeinsamer bewerber" = "joint_list",
  "gemeinsamer bewerberin" = "joint_list",
  "gemeinsame bewerberin" = "joint_list",
  "gemeinsamer kandidat" = "joint_list",
  # alone a local list; beside a party ("CDU - Offene Liste") no family
  "offene liste" = "fw_wg"
)

pf_party_patterns <- c(
  cdu_csu   = paste0("\\bcdu\\b|\\bcsu\\b|christlich[- ]demokratische[- ]union|",
                     "christlich[- ]soziale[- ]union|\\bju\\b|junge union|frauen ?union|",
                     "senioren ?union|mittelstandsunion"),
  spd       = "\\bspd\\b|sozialdemokratische partei",
  gruene    = "\\bgrüne?n?\\b|\\bgruene?n?\\b|bündnis ?90|\\bb ?90\\b|\\bgal\\b|grün[- ]alternativ",
  linke_pds = "\\bpds\\b|\\blinke\\b|\\blinkspartei\\b",
  fdp       = "\\bfdp\\b|\\bdvp\\b|freie demokratische partei|freie demokraten",
  afd       = "\\bafd\\b|alternative für deutschland"
)

# Other named parties: "other:<key>".
pf_other_patterns <- c(
  bp = "\\bbp\\b|bayernpartei",
  oedp = "\\bödp\\b|ökologisch[- ]demokratische",
  npd_heimat = "\\bnpd\\b|\\bdie heimat\\b",
  dvu = "\\bdvu\\b",
  # parties of the 1950s-60s (Bayern)
  kpd = "\\bkpd\\b",
  bhe_gdp = "\\bbhe\\b|\\bgb\\b|\\bgdp\\b|gesamtdeutsche",
  wav = "\\bwav\\b",
  dp = "\\bdp\\b|deutsche partei",
  drp_srp = "\\bdrp\\b|\\bsrp\\b|deutsche reichspartei",
  rep = "\\brep\\b|republikaner",
  piraten = "\\bpiraten",
  bsw = "\\bbsw\\b|bündnis sahra wagenknecht",
  die_partei = "\\bdie partei\\b",
  tierschutz = "tierschutz",
  ssw = "\\bssw\\b",
  dkp = "\\bdkp\\b",
  mlpd = "\\bmlpd\\b",
  volt = "\\bvolt\\b",
  diebasis = "\\bdie ?basis\\b",
  werteunion = "werteunion",
  freie_sachsen = "freie sachsen",
  familie = "familien[- ]?partei",
  zentrum = "\\bzentrum\\b",
  dsu = "\\bdsu\\b",
  schill = "\\bschill\\b|rechtsstaatlicher offensive",
  statt = "^statt partei$",
  biw = "\\bbiw\\b|bürger in wut",
  iii_weg = "iii weg|dritte weg",
  humanisten = "humanisten",
  # any other "... Partei ..." (not parteilos / überparteilich: whole word;
  # not "Frei ohne Partei", "Bürger statt Partei": local lists)
  partei = "(?<!ohne )(?<!statt )\\bpartei\\b"
)

# A component that is an independent candidature (start of the component).
pf_independent <- paste0(
  "^(eb|ebw|ewv|ezb|einzelbewerber(in)?|einzelbewerbung|einzelbew|",
  "einzelwahlvorschlag|einwahlvorschlag|einzelvorschlag|einzelkandidat(in)?|",
  "kein wahlvorschlag|ohne wahlvorschlag|weitere personen|andere personen)\\b|",
  # whole component only: "Parteilose Wählergruppe" is a local list
  "^(parteilos|parteifrei|unabhängig)(e|er)?( kandidat(in)?| bewerber(in)?)?$|^ohne partei$|^part ?fr$"
)

# Components that carry no family ("e.V.", "gemeinsamer Wahlvorschlag").
pf_filler <- paste0(
  "^(|e ?v|die|der|liste|wahlvorschlag|gemeinsamer( wahl)?vorschlag|gemeinsame liste|u ?a|sonstige)$|",
  # a party's local branch or following: "SPD - Ortsverein", "CDU - Offene
  # Liste", "CSU und ihre Freunde". (Not "SV"/"KV": in Thüringen sports and
  # carnival clubs nominate candidates.)
  "^(ortsverband|ortsverein|ortsgruppe|ortsunion|kreisverband|stadtverband|gemeindeverband)\\b|",
  "^(offene liste|(ihre )?(freunde|anhänger|nahestehende|sympathisanten|parteifreunde)|andere)$"
)

pf_norm <- function(x) {
  y <- gsub('""', '"', as.character(x), fixed = TRUE)
  y <- tolower(trimws(y))
  y <- gsub(".", "", y, fixed = TRUE)            # F.D.P., e.V., Wählergr.
  y <- gsub("[\"'„“”‚‘’`()]", " ", y)
  y <- gsub("[[:space:]]+", " ", trimws(y))
  y
}

# One component -> its family keys.
pf_component <- function(t) {
  t <- trimws(t)
  hit <- names(pf_party_patterns)[vapply(paste0("(*UCP)", pf_party_patterns), grepl,
                                         logical(1), x = t, perl = TRUE)]
  if (length(hit)) return(hit)
  oth <- names(pf_other_patterns)[vapply(paste0("(*UCP)", pf_other_patterns), grepl,
                                         logical(1), x = t, perl = TRUE)]
  if (length(oth)) return(paste0("other:", oth[1]))
  if (grepl(paste0("(*UCP)", pf_independent), t, perl = TRUE)) return("independent")
  if (grepl(paste0("(*UCP)", pf_filler), t, perl = TRUE)) return(character(0))
  "fw_wg"
}

# Is the label the candidate's own name (with at most an independent prefix)?
pf_own_name <- function(label, last, first) {
  if (is.na(label) || is.na(last) || !nzchar(trimws(last))) return(FALSE)
  if (!grepl(last, label, fixed = TRUE)) return(FALSE)
  rest <- gsub(last, " ", label, fixed = TRUE)
  given <- if (is.na(first)) character(0) else strsplit(first, "[[:space:]-]+")[[1]]
  for (g in given[nzchar(given)]) rest <- gsub(g, " ", rest, fixed = TRUE)
  rest <- gsub("\\b(Dr|Prof)\\b\\.?", " ", rest, perl = TRUE)
  rest <- gsub("\\b[[:upper:]]\\.", " ", rest, perl = TRUE)
  rest <- tolower(gsub("[^[:alpha:]]+", "", rest))
  rest %in% c("", "eb", "ebw", "ew", "ewv", "ev", "einzelbewerber", "einzelbewerberin",
              "einzelwahlvorschlag", "einwahlvorschlag", "einzelvorschlag")
}

party_family_parts <- function(label, state, last = NULL, first = NULL) {
  n <- length(label)
  if (is.null(last)) last <- rep(NA_character_, n)
  if (is.null(first)) first <- rep(NA_character_, n)
  state <- rep_len(as.character(state), n)
  key <- paste(label, state, last, first, sep = "\r")
  u <- !duplicated(key)
  res_u <- vapply(which(u), function(i) {
    lab <- label[i]
    if (is.na(lab)) return(NA_character_)
    # Sachsen-Anhalt prints no Wahlvorschlag for an Einzelbewerber
    if (!nzchar(trimws(lab))) {
      return(if (state[i] == "15") "independent" else NA_character_)
    }
    if (pf_own_name(lab, last[i], first[i])) return("independent")
    y <- pf_norm(lab)
    if (y %in% names(pf_pinned)) return(unname(pf_pinned[y]))
    comps <- strsplit(y, "/|,|;|\\+|&| und | - | u ", perl = TRUE)[[1]]
    fam <- unique(unlist(lapply(comps, pf_component)))
    if (length(fam) > 1) fam <- setdiff(fam, "independent")
    if (!length(fam)) return(NA_character_)
    paste(sort(fam), collapse = "|")
  }, character(1))
  res_u[match(key, key[u])]
}

party_family_from_parts <- function(parts) {
  out <- ifelse(grepl("|", parts, fixed = TRUE), "joint_list",
                sub(":.*$", "", parts))
  out[is.na(parts)] <- NA_character_
  out
}

party_family <- function(label, state, last = NULL, first = NULL) {
  party_family_from_parts(party_family_parts(label, state, last, first))
}

# Do two component strings share a family? (NA if either is unknown)
party_parts_overlap <- function(a, b) {
  out <- mapply(function(x, y) {
    if (is.na(x) || is.na(y) || x == "joint_list" || y == "joint_list") return(NA)
    length(intersect(strsplit(x, "|", fixed = TRUE)[[1]],
                     strsplit(y, "|", fixed = TRUE)[[1]])) > 0
  }, a, b, USE.NAMES = FALSE)
  as.logical(out)
}

# Within one municipality's elections in date order: 1 when none of the
# components has won before (the first election included), 0 otherwise or when
# the family is unknown.
party_parts_new <- function(parts) {
  seen <- character(0)
  out <- integer(length(parts))
  for (i in seq_along(parts)) {
    if (is.na(parts[i]) || parts[i] == "joint_list") next
    comp <- strsplit(parts[i], "|", fixed = TRUE)[[1]]
    out[i] <- as.integer(!any(comp %in% seen))
    seen <- union(seen, comp)
  }
  out
}

# Party family of an election-level file's winner_party (mayoral_unharm,
# landrat_unharm: one row per round, winner_party = the round's winner or
# leader). The own-name rule needs the person, so the label is matched to the
# candidate of that round who carries it (`cands` in the wide candidate
# schema: election_date / election_date_sw, candidate_party, names, votes; the
# top-voted one if two share the label). A round without such a candidate gets
# the label's family alone.
party_family_rounds <- function(rounds, cands) {
  s <- !is.na(cands$election_date_sw)
  nm <- data.frame(
    k = c(paste(cands$ags, cands$election_date, cands$candidate_party, sep = "\r"),
          paste(cands$ags[s], cands$election_date_sw[s], cands$candidate_party[s], sep = "\r")),
    last = c(cands$candidate_last_name, cands$candidate_last_name[s]),
    first = c(cands$candidate_first_name, cands$candidate_first_name[s]),
    v = c(cands$candidate_votes_hw, cands$candidate_votes_sw[s]),
    ok = c(!is.na(cands$candidate_party) & !is.na(cands$candidate_last_name),
           !is.na(cands$candidate_party[s]) & !is.na(cands$candidate_last_name[s])),
    stringsAsFactors = FALSE)
  nm <- nm[nm$ok, ]
  nm <- nm[order(is.na(nm$v), -nm$v), ]
  nm <- nm[!duplicated(nm$k), ]
  i <- match(paste(rounds$ags, rounds$election_date, rounds$winner_party, sep = "\r"), nm$k)
  party_family(rounds$winner_party, substr(as.character(rounds$ags), 1, 2),
               nm$last[i], nm$first[i])
}
