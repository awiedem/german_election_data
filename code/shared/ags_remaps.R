### AGS corrections shared by the county and municipal harmonisation scripts
#
# Source codes that appear in NO vintage of ags_crosswalks and therefore cannot
# be joined at all. Each is remapped to a code that does exist and that resolves
# to the same 2021 municipality.
#
# ---------------------------------------------------------------------------
# WHY THIS FILE HOLDS ONLY EIGHT ROWS
# ---------------------------------------------------------------------------
# The July-2026 audit worklist proposed merging the two pipelines' correction
# tables outright. That turned out to be wrong: they deliberately use DIFFERENT
# strategies for the Sachsen-Anhalt cases, and both are correct in context.
#
#   county 02  maps FORWARD to the post-merger code that exists in the election
#              year   (Biere 15089040/2007 -> 15089042 Bördeland, valid 2007-2020)
#   municipal 02 maps BACK to the pre-merger historical code and joins at an
#              earlier crosswalk vintage via year_cw
#              (Biere 15089040/2007 -> 15367003, valid 1994-2006)
#
# Both land on ags_21 = 15089042. Merging them would force one pipeline onto the
# other's join strategy for no benefit, so the Sachsen-Anhalt entries stay where
# they are. Only the rows that are already IDENTICAL in both scripts live here —
# these are the ones that had to be hand-ported when new data was ingested and
# where silent divergence was the real risk.
#
# The divergence was worth checking rather than assuming: it turned up a genuine
# defect. The municipal table sent Eickendorf 15089085/2007 to 15362031, which is
# the OTHER Eickendorf — the Ohrekreis one, resolving to Oebisfelde-Weferlingen
# — instead of 15367008, the Schönebeck Eickendorf that sits alongside its
# siblings Biere 15367003 and Welsleben 15367027 in Bördeland. Sachsen-Anhalt
# 2007 is not in municipal_unharm today, so it was unreachable, but it would have
# filed a real election under the wrong municipality the day that data arrives.
#
# ---------------------------------------------------------------------------
# Columns: ags (source, 8 char), election_year, ags_new (target), note
ags_remaps_shared <- data.frame(
  ags = c(
    # Thüringen 1994: seven municipalities dissolved before the crosswalk's
    # coverage begins; they appear under no later code.
    "16063047", "16063056", "16063057", "16068054",
    "16069022", "16073098", "16074023",
    # Mecklenburg-Vorpommern 2004: Prebberede is recorded under a code that is
    # in no crosswalk year; its own code is 13053109 (both resolve to 13072082).
    "13053108"
  ),
  election_year = c(rep(1994L, 7), 2004L),
  ags_new = c(
    "16016410", "16015420", "16063094", "16018580",
    "16023360", "16033700", "16041070",
    "13053109"
  ),
  note = c("Kupfersuhl", "Moehra", "Moorgrund", "Toettelstaedt",
           "Hessberg", "Weissen", "Gernewitz", "Prebberede"),
  stringsAsFactors = FALSE
)

#' Apply the shared AGS corrections to a data frame
#'
#' @param df    data frame with character `ags` and numeric `election_year`
#' @param label pipeline name, used only in the message
#' @return `df` with corrected `ags`
apply_ags_remaps_shared <- function(df, label = "") {
  key <- paste(df$ags, df$election_year)
  map <- setNames(ags_remaps_shared$ags_new,
                  paste(ags_remaps_shared$ags, ags_remaps_shared$election_year))
  hit <- key %in% names(map)
  if (any(hit)) {
    df$ags[hit] <- unname(map[key[hit]])
    cat(sprintf("  %sapplied %d shared AGS correction(s)\n",
                if (nzchar(label)) paste0(label, ": ") else "", sum(hit)))
  }
  df
}
