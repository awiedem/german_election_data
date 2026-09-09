# Internal helper for export_excel.py: use RDS column classes rather than guessing
# identifiers and dates from CSV values. Run with Rscript --vanilla.
args <- commandArgs(trailingOnly = TRUE)
paths <- readLines(args[[1]], warn = FALSE)
out <- list()
for (path in paths) {
  x <- readRDS(path)
  if (!is.data.frame(x)) stop("Not a table: ", path)
  kinds <- vapply(x, function(column) {
    if (inherits(column, "Date")) return("date")
    if (inherits(column, "POSIXt")) return("datetime")
    if (is.character(column) || is.factor(column)) return("text")
    if (is.logical(column)) return("boolean")
    if (is.numeric(column)) return("number")
    stop("Unsupported column class in ", path, ": ", paste(class(column), collapse = "/"))
  }, character(1))
  out[[path]] <- list(rows = nrow(x), columns = as.list(kinds))
  if (!file.exists(sub("\\.rds$", ".csv", path))) {
    # RDS-only public crosswalks get a temporary CSV, never a new source output.
    utils::write.table(x, file.path(args[[3]], paste0(basename(path), ".csv")),
                       sep = ",", row.names = FALSE, col.names = TRUE,
                       na = "", qmethod = "double", fileEncoding = "UTF-8")
  }
  rm(x)
}
jsonlite::write_json(out, args[[2]], auto_unbox = TRUE, pretty = TRUE)
