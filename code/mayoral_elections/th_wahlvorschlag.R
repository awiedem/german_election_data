# Thüringen Wahlvorschlag strings (wahlen.thueringen.de), shared by 01 and 01b.
#
# The database redacts candidates' personal data (§ 50 Abs. 2 ThürKWO) and lists
# each candidate by the Wahlvorschlag only ("CDU", "Einzelbewerber", "Weitere
# Personen"). Where it names a person -- always the elected person, and every
# candidate of a recent election -- the Wahlvorschlag reads "Nachname, Vorname"
# or "Nachname, Vorname (Träger)". 00_th_scrape.py stores that string whole as
# candidate_party, so the party field held a name (known issues §16).
#
# split_th_wahlvorschlag() returns, per string:
#   person  TRUE if the string names a person;
#   name    "Nachname, Vorname" without the bracket (NA if not a person);
#   party   the Träger in the bracket for a person (NA when the source gives
#           none), else the string unchanged.
# The bracket may itself contain brackets ("Bürger für Bürger (BfB)") or lack
# its closing one. Joint lists ("SPD, CDU, UWS", "FFW, ATG") are not persons:
# every name word must be capitalised and lower-case after the first letter.

# Wahlvorschläge that look like "Nachname, Vorname" but are groups (checked by hand).
TH_WAHLVORSCHLAG_NOT_PERSON <- c(
  "Bi, Zukunft Kammerforst"   # Bürgerinitiative, Kammerforst 2019
)

split_th_wahlvorschlag <- function(w) {
  w <- trimws(w)
  has_br <- !is.na(w) & grepl(" \\(", w)
  core <- ifelse(has_br, trimws(sub(" \\(.*$", "", w)), w)
  rest <- ifelse(has_br, sub("^[^(]* \\(", "", w), NA_character_)
  n_open <- lengths(regmatches(rest, gregexpr("\\(", rest)))
  n_close <- lengths(regmatches(rest, gregexpr("\\)", rest)))
  traeger <- ifelse(has_br & n_close > n_open, trimws(sub("\\)\\s*$", "", rest)), trimws(rest))
  tok <- "\\p{Lu}[\\p{Ll}ß']+"
  word <- paste0("(?:", tok, "|von|van|de|zu|vom|zum|zur|der|den|ten|ter)")
  surname <- paste0("(?:(?:Dr|Prof)\\.(?: [a-z]+\\.)* )*", word, "(?:[ -]", word, ")*")
  given <- paste0(tok, "(?:-", tok, ")*(?: (?:", tok, "(?:-", tok, ")*|\\p{Lu}\\.))*")
  person <- !is.na(core) &
    grepl(paste0("^", surname, ", ", given, "$"), core, perl = TRUE) &
    grepl("\\p{Lu}", sub(",.*$", "", core), perl = TRUE) &
    !core %in% TH_WAHLVORSCHLAG_NOT_PERSON
  list(person = person,
       name   = ifelse(person, core, NA_character_),
       party  = ifelse(person, ifelse(is.na(traeger) | traeger == "", NA_character_, traeger), w))
}
