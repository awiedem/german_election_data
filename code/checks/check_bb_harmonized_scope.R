# Compare staged before/after 1990/1994 rebuilds, including cross-state recipients.
# Rscript --vanilla code/checks/check_bb_harmonized_scope.R BEFORE_RDS_DIR AFTER_RDS_DIR BEFORE_LINEAGE_DIR AFTER_LINEAGE_DIR OUTPUT_DIR
library(data.table)
set.seed(20260928)
args <- commandArgs(trailingOnly = TRUE)
stopifnot(length(args) == 5L)
dir.create(args[[5]], recursive = TRUE, showWarnings = FALSE)
key <- function(d) paste(d$ags, d$election_year, sep = "/")
changes <- list(); summaries <- list(); missingness <- list()
for (target in c("21", "23", "25")) {
  stem <- paste0("state_harm_", target)
  before <- readRDS(file.path(args[[1]], paste0(stem, ".rds")))
  after <- readRDS(file.path(args[[2]], paste0(stem, ".rds")))
  stopifnot(!anyDuplicated(key(before)), !anyDuplicated(key(after)),
            all(before$election_year %in% c(1990, 1994)),
            all(after$election_year %in% c(1990, 1994)),
            identical(setdiff(names(after), names(before)), "domowina"),
            !length(setdiff(names(before), names(after))))
  lineage <- rbindlist(lapply(args[3:4], function(directory) {
    fread(file.path(directory, paste0(stem, "_provenance.csv")),
          colClasses = c(state = "character", ags = "character",
                         ags_original = "character"))
  }), fill = TRUE)
  target_col <- paste0("ags_", target)
  allowed <- unique(paste(lineage[[target_col]][lineage$state == "12"],
                          lineage$election_year[lineage$state == "12"], sep = "/"))
  stopifnot(all(substr(allowed, 1, 2) %in% c("12", "13")))
  before$domowina <- NA_real_
  common <- intersect(key(before), key(after))
  a <- before[match(common, key(before)), names(after)]
  b <- after[match(common, key(after)), ]
  different <- lapply(names(after), function(nm) {
    x <- a[[nm]]; y <- b[[nm]]
    is.na(x) != is.na(y) | (!is.na(x) & !is.na(y) & x != y)
  })
  changed <- Reduce(`|`, different)
  changed_keys <- common[changed]
  added <- setdiff(key(after), key(before))
  removed <- setdiff(key(before), key(after))
  stopifnot(all(c(changed_keys, added, removed) %in% allowed))
  fields <- vapply(which(changed), function(i) {
    paste(names(after)[vapply(different, `[`, logical(1), i)], collapse = ",")
  }, character(1))
  keys <- c(changed_keys, added, removed)
  detail <- data.table(dataset = stem, target_key = keys,
                       state = substr(keys, 1, 2),
                       election_year = as.integer(sub(".*/", "", keys)),
                       status = c(rep("changed", length(changed_keys)),
                                  rep("added", length(added)), rep("removed", length(removed))),
                       fields = c(fields, rep("all", length(added) + length(removed))))
  changes[[stem]] <- detail
  summaries[[stem]] <- detail[, .(rows = .N), by = .(dataset, state, election_year, status)]
  for (state in c("12", "13")) for (year in c(1990, 1994)) {
    x <- before[before$state == state & before$election_year == year & key(before) %in% allowed, ]
    y <- after[after$state == state & after$election_year == year & key(after) %in% allowed, ]
    for (field in names(after)) {
      na_before <- sum(is.na(x[[field]])); na_after <- sum(is.na(y[[field]]))
      if (na_before != na_after) missingness[[length(missingness) + 1L]] <- data.table(
        dataset = stem, state = state, election_year = year, field = field,
        rows_before = nrow(x), rows_after = nrow(y),
        missing_before = na_before, missing_after = na_after)
    }
  }
  cat(sprintf("PASS %s: %d changed, %d added, %d removed; %d unrelated/common rows identical.\n",
              stem, length(changed_keys), length(added), length(removed), sum(!changed)))
}
fwrite(rbindlist(changes), file.path(args[[5]], "modern_changed_keys.tsv"), sep = "\t")
fwrite(rbindlist(summaries), file.path(args[[5]], "modern_changed_summary.tsv"), sep = "\t")
fwrite(rbindlist(missingness), file.path(args[[5]], "modern_missingness_changes.tsv"), sep = "\t")
