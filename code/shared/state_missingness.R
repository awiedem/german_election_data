# Sum observed counts, but never turn an entirely unknown group into zero.
# Partial groups still return the sum of known observations, not an estimate
# of a complete total. Consult the per-election completeness metadata.
gerda_sum_known <- function(x) {
  if (all(is.na(x))) NA_real_ else sum(x, na.rm = TRUE)
}
