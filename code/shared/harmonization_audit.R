# Source-row accounting before any unmatched rows can be filtered or aggregated.
# These checks compare against the input, never just against surviving matches.

gerda_audit_write <- function(x, label, suffix,
                             directory = getOption("gerda.audit_dir", "data/data_checks/harmonization")) {
  dir.create(directory, recursive = TRUE, showWarnings = FALSE)
  path <- file.path(directory, paste0(gsub("[^A-Za-z0-9_-]", "_", label), "_", suffix, ".csv"))
  data.table::fwrite(as.data.frame(x), path, na = "NA")
  invisible(path)
}

gerda_key <- function(x, keys) {
  stopifnot(length(keys) > 0L, all(keys %in% names(x)))
  parts <- lapply(as.data.frame(x)[keys], as.character)
  if (any(vapply(parts, function(v) anyNA(v) || any(!nzchar(v)) ||
                 any(grepl("\r", v, fixed = TRUE)), logical(1)))) {
    stop("Missing/empty source key or reserved separator in key")
  }
  do.call(paste, c(parts, sep = "\r"))
}

# A composed crosswalk can reach the same target by several distinct routes.
# Sum those route weights before joining election results. Identical duplicate
# rows need a provenance review before they can be treated as distinct routes.
gerda_collapse_crosswalk <- function(cw, weights = c("pop_cw", "area_cw", "emp_cw")) {
  cw <- data.table::as.data.table(cw)
  if (anyDuplicated(cw)) stop("Identical duplicate crosswalk rows")
  weights <- intersect(weights, names(cw))
  stopifnot(length(weights) > 0L)
  by <- setdiff(names(cw), weights)
  tibble::as_tibble(cw[, lapply(.SD, sum), by = by, .SDcols = weights])
}

# Exclusions are keyed to particular observations, with a reason and evidence.
# No AGS-wide allowlist, and no unknown target may enter a harmonized output.
gerda_exclude_documented <- function(source, exceptions, keys, label) {
  stopifnot(all(c(keys, "reason", "evidence") %in% names(exceptions)),
            !anyNA(exceptions$reason), all(nzchar(exceptions$reason)),
            !anyNA(exceptions$evidence), all(nzchar(exceptions$evidence)))
  ek <- gerda_key(exceptions, keys)
  stopifnot(!anyDuplicated(ek))
  index <- match(gerda_key(source, keys), ek)
  drop <- !is.na(index)
  cols <- unique(c(keys, intersect(c("ags_name", "state", "election_year",
    "eligible_voters", "number_voters", "valid_votes", "invalid_votes"), names(source))))
  report <- as.data.frame(source[drop, cols, drop = FALSE])
  report$reason <- exceptions$reason[index[drop]]
  report$evidence <- exceptions$evidence[index[drop]]
  gerda_audit_write(report, label, "exclusions")
  source[!drop, , drop = FALSE]
}

gerda_require_mapped <- function(x, column, label) {
  stopifnot(column %in% names(x))
  bad <- is.na(x[[column]]) | !nzchar(as.character(x[[column]]))
  if (any(bad)) {
    cols <- unique(c(column, intersect(c("ags", "election_year", "name", "county_prefix",
      "gemeindename", "eligible_voters", "number_voters", "valid_votes"), names(x))))
    report <- as.data.frame(x[bad, cols, drop = FALSE])
    report$source_row <- which(bad)
    path <- gerda_audit_write(report, label, "unmapped")
    stop(label, ": unresolved source rows; diagnostic: ", path, call. = FALSE)
  }
  invisible(TRUE)
}

# Validate keys before a year/type filter can silently remove an NA-key row.
gerda_read_election_source <- function(path, keys = c("ags", "election_year")) {
  x <- readRDS(path)
  for (key in keys) gerda_require_mapped(x, key, paste0(tools::file_path_sans_ext(basename(path)), "_input"))
  x
}

# For inner joins that allocate postal totals or attach indispensable lookup
# values: every source key must reach at least one recipient. Splits are allowed.
gerda_require_join_coverage <- function(source, recipients, keys, label) {
  sk <- gerda_key(source, keys); rk <- gerda_key(recipients, keys)
  lost <- !sk %in% rk
  if (any(lost)) {
    path <- gerda_audit_write(as.data.frame(source[lost, keys, drop = FALSE]), label, "join_failures")
    stop(label, ": inner join would discard source rows; diagnostic: ", path, call. = FALSE)
  }
  invisible(TRUE)
}

# A nearest-year lookup may have two equally close years. The old 0.001
# preference for a later year hid conflicting mappings. Accept a tie only
# when both complete target/weight vectors agree; otherwise require review.
gerda_nearest_crosswalk_year <- function(keys, cw, target, label) {
  weights <- intersect(c("pop_cw", "area_cw"), names(cw))
  cw <- as.data.frame(cw)
  candidates <- split(cw[cw$ags %in% keys$ags,
                         c("ags", "election_year", target, weights), drop = FALSE],
                      cw$ags[cw$ags %in% keys$ags])
  result <- as.data.frame(keys)
  result$cw_year <- rep(NA_real_, nrow(result))
  for (i in seq_len(nrow(result))) {
    choices <- candidates[[result$ags[i]]]
    if (is.null(choices) || !nrow(choices)) next
    distance <- abs(choices$election_year - result$election_year[i])
    near <- choices[distance == min(distance), , drop = FALSE]
    years <- sort(unique(near$election_year))
    if (length(years) > 1L) {
      vectors <- lapply(years, function(year) {
        x <- near[near$election_year == year, c(target, weights), drop = FALSE]
        x <- x[order(x[[target]]), , drop = FALSE]; rownames(x) <- NULL; x
      })
      same <- vapply(vectors[-1], function(x) isTRUE(all.equal(vectors[[1]], x,
                     tolerance = 1e-7, check.attributes = FALSE)), logical(1))
      if (!all(same)) {
        near$source_election_year <- result$election_year[i]
        path <- gerda_audit_write(near, label, "ambiguous_years")
        stop(label, ": equally close crosswalk years disagree; diagnostic: ", path)
      }
    }
    result$cw_year[i] <- max(years)
  }
  tibble::as_tibble(result[!is.na(result$cw_year), , drop = FALSE])
}

gerda_audit_mapping <- function(source, mapped, keys, target, label,
                                weight = "pop_cw", target_codes = NULL,
                                counts = c("eligible_voters", "number_voters", "valid_votes", "invalid_votes"),
                                target_width = 8L, allow_zero_weight_sum = FALSE,
                                tolerance = 1e-7) {
  source <- as.data.frame(source)
  mapped <- as.data.frame(mapped)
  stopifnot(target %in% names(mapped), is.null(weight) || weight %in% names(mapped))
  stopifnot(is.character(mapped[[target]]), is.null(weight) || is.numeric(mapped[[weight]]))
  counts <- intersect(counts, names(source))
  stopifnot(all(counts %in% names(mapped)))
  checked_key <- function(x, which) tryCatch(gerda_key(x, keys), error = function(e) {
    path <- gerda_audit_write(x[intersect(keys, names(x))], label, paste0("invalid_", which, "_keys"))
    stop(conditionMessage(e), "; diagnostic: ", path, call. = FALSE)
  })
  sk <- checked_key(source, "source")
  mk <- checked_key(mapped, "mapping")
  if (!nrow(source) && !nrow(mapped)) {
    gerda_audit_write(data.frame(reason = character()), label, "failures")
    gerda_audit_write(source[keys], label, "source_ledger")
    gerda_audit_write(mapped[unique(c(keys, target, weight))], label, "mapping")
    return(invisible(source[keys]))
  }
  if (anyDuplicated(sk)) {
    bad <- source[duplicated(sk) | duplicated(sk, fromLast = TRUE), keys, drop = FALSE]
    gerda_audit_write(bad, label, "duplicate_source_keys")
    stop(label, ": source keys are not unique; preserve the original source identity")
  }
  index <- match(mk, sk)
  issues <- list()
  add_issue <- function(rows, reason) {
    if (nrow(rows)) { rows$reason <- reason; issues[[length(issues) + 1L]] <<- rows }
  }
  report_counts <- intersect(counts, c("eligible_voters", "number_voters", "valid_votes", "invalid_votes"))
  detail_cols <- unique(c(keys, intersect(c("ags", "ags_name", "state", "election_year", "election_date"), names(source)), report_counts))
  add_issue(source[!sk %in% mk, detail_cols, drop = FALSE], "source_row_disappeared")
  bad_target <- is.na(mapped[[target]]) | !grepl(paste0("^[0-9]{", target_width, "}$"), mapped[[target]])
  if (!is.null(target_codes)) bad_target <- bad_target | !mapped[[target]] %in% target_codes
  mapped_detail <- unique(c(keys, target, intersect(c(weight, report_counts), names(mapped))))
  add_issue(mapped[bad_target, mapped_detail, drop = FALSE], "missing_or_invalid_target")
  add_issue(mapped[is.na(index), mapped_detail, drop = FALSE], "mapping_has_unknown_source")
  duplicate <- duplicated(mapped[c(keys, target)]) | duplicated(mapped[c(keys, target)], fromLast = TRUE)
  add_issue(mapped[duplicate, mapped_detail, drop = FALSE], "duplicate_source_target_edge")
  w <- if (is.null(weight)) rep(1, nrow(mapped)) else mapped[[weight]]
  bad_weight <- !is.finite(w) | w < 0
  add_issue(mapped[bad_weight, mapped_detail, drop = FALSE], "missing_nonfinite_or_negative_weight")
  # Source values must survive joins unchanged, including missingness. A join
  # that duplicates only some rows or replaces NA with zero must fail here.
  for (column in counts) {
    a <- source[[column]][index]; b <- mapped[[column]]
    changed <- xor(is.na(a), is.na(b)) |
      (!is.na(a) & !is.na(b) & (!is.finite(b) | abs(a - b) > tolerance * pmax(1, abs(a))))
    changed[is.na(changed)] <- TRUE
    add_issue(mapped[changed, mapped_detail, drop = FALSE], paste0("source_value_changed:", column))
  }
  ledger <- source[detail_cols]
  ledger$mapping_rows <- tabulate(index, nbins = nrow(source))
  totals <- tapply(w, factor(index, levels = seq_len(nrow(source))), sum)
  ledger$weight_sum <- as.numeric(totals)
  bad_sum <- !is.finite(ledger$weight_sum) | abs(ledger$weight_sum - 1) > tolerance
  # A few historical crosswalk entries aggregate uninhabited territories. This
  # explicit exception can only apply when EVERY available count is known zero.
  zero <- rep(FALSE, nrow(source))
  if (allow_zero_weight_sum && length(counts)) {
    zero <- Reduce(`&`, lapply(source[counts], function(v) !is.na(v) & v == 0))
  }
  ledger$status <- ifelse(bad_sum & zero, "zero_count_weight_exception", "mapped")
  add_issue(ledger[bad_sum & !zero, , drop = FALSE], "weights_do_not_sum_to_one")
  # A missing edge with zero votes remains an error (bad target / disappeared).
  if (length(issues)) {
    failures <- data.table::rbindlist(issues, fill = TRUE)
    path <- gerda_audit_write(failures, label, "failures")
    gerda_audit_write(ledger, label, "source_ledger")
    stop(label, ": invalid/incomplete mapping; diagnostic: ", path, call. = FALSE)
  }
  # Clear stale failure reports only after a successful run.
  gerda_audit_write(data.frame(reason = character()), label, "failures")
  gerda_audit_write(ledger, label, "source_ledger")
  provenance <- unique(c(keys, target, weight,
    intersect(c("ags_original", "crosswalk_year", "mapping_method", "source_boundary_year", "year_cw", "cw_year"), names(mapped))))
  gerda_audit_write(mapped[provenance], label, "mapping")
  invisible(ledger)
}

# Check the actual aggregated output, including known-vs-missing counts. Run
# before rounding; crosswalk allocation may yield fractional counts.
gerda_audit_totals <- function(source, output, groups, counts, label, tolerance = 1e-7) {
  aggregate_counts <- function(x) {
    x <- data.table::as.data.table(x)
    x[, lapply(.SD, function(v) if (all(is.na(v))) NA_real_ else sum(v, na.rm = TRUE)),
      by = groups, .SDcols = counts]
  }
  a <- aggregate_counts(source); b <- aggregate_counts(output)
  ak <- gerda_key(a, groups); bk <- gerda_key(b, groups)
  if (!setequal(ak, bk)) {
    missing <- as.data.frame(a[!ak %in% bk, ..groups]); missing$reason <- rep("missing_output_group", nrow(missing))
    extra <- as.data.frame(b[!bk %in% ak, ..groups]); extra$reason <- rep("unexpected_output_group", nrow(extra))
    path <- gerda_audit_write(data.table::rbindlist(list(missing, extra)), label, "conservation_failures")
    stop(label, ": aggregation added/lost an entire group; diagnostic: ", path)
  }
  b <- b[match(ak, bk)]
  differences <- list()
  for (column in counts) {
    before <- a[[column]]; after <- b[[column]]
    bad <- xor(is.na(before), is.na(after)) |
      (!is.na(before) & !is.na(after) & (!is.finite(before) | !is.finite(after) |
        abs(after - before) > tolerance * pmax(1, abs(before))))
    bad[is.na(bad)] <- TRUE
    if (any(bad)) differences[[column]] <- cbind(as.data.frame(a[bad, ..groups]),
      data.frame(column, before = before[bad], after = after[bad]))
  }
  report <- if (length(differences)) data.table::rbindlist(differences) else data.frame(column = character())
  path <- gerda_audit_write(report, label, "conservation_failures")
  if (nrow(report)) stop(label, ": counts not conserved; diagnostic: ", path, call. = FALSE)
  invisible(TRUE)
}

# Chunk columns to avoid hundreds of R callbacks for every municipality-year.
# Missing counts remain missing when every contributing value is missing.
gerda_weighted_counts <- function(x, keys, columns, weight) {
  x <- as.data.frame(x)
  k <- gerda_key(x, keys)
  if (!nrow(x) || !length(columns)) return(tibble::as_tibble(x[unique(c(keys, columns))]))
  groups <- match(k, unique(k))
  result <- x[!duplicated(k), keys, drop = FALSE]
  for (start in seq.int(1L, length(columns), by = 32L)) {
    cols <- columns[start:min(start + 31L, length(columns))]
    v <- as.matrix(x[cols])
    known <- rowsum((!is.na(v)) * 1L, groups, reorder = FALSE)
    totals <- rowsum(v * x[[weight]], groups, reorder = FALSE, na.rm = TRUE)
    totals[known == 0L] <- NA_real_
    result[cols] <- as.data.frame(totals)
  }
  tibble::as_tibble(result)
}
