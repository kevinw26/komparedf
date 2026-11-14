.glue <- \(...) glue(..., .trim = FALSE, .envir = parent.frame())

# count with the locale thousands separator
f_num <- \(x) {
  format(x, big.mark = Sys.localeconv()[["thousands_sep"]], scientific = FALSE)
}

.print_diffs <- \(d) {
  lines <- capture.output(print(d)) |>
    discard(\(x) str_detect(str_remove_all(x, "\x1b\\[[0-9;]*m"), "^\\s*(<|#)"))
  str_c("  ", lines) |>
    str_c(collapse = "\n") |>
    cat("\n")
}

#' summarize a kompare object
#'
#' @param object a `kompare` object, as returned by [kompare()].
#' @param ... not used.
#'
#' @return invisibly returns `object`.
#'
#' @importFrom dplyr bind_rows count pull
#' @importFrom stringr str_c str_detect str_remove_all str_length
#' @importFrom purrr discard imap iwalk map map_int
#' @importFrom glue glue
#' @export
summary.kompare <- \(object, ...) {
  "<kompare>\n" |> cat()
  # schema mismatches: just report the count, then list affected columns
  .glue("schema mismatches: {nrow(object$schema)}\n") |> cat()
  if (nrow(object$schema) > 0) {
    cols <- object$schema |>
      head(10) |>
      .print_diffs()
    if (nrow(object$schema) > 10) .glue("  # ... and {nrow(object$schema) - 10} more\n") |> cat()
    cat("\n")
  }

  .glue("unmatched keys: {nrow(object$keys)}\n") |> cat()
  if (nrow(object$keys) > 0) {
    cols <- object$keys |>
      head(5) |>
      .print_diffs()
    if (nrow(object$keys) > 5) .glue("  # ... and {nrow(object$keys) - 5} more\n") |> cat()
    cat("\n")
  }

  # value differences: show the first five columns per type, first five
  # rows of differences for those columns, printed with the tibble printer
  if (length(object$diffs) == 0) {
    "value differences: 0\n" |> cat()
  } else {
    "value differences by type (up to five of each type shown):\n" |> cat()
    shown_diffs <- object$diffs |>
      imap(\(d, type) d |>
        head(5) |>
        mutate(type = type, .before = 1) %>%
        mutate(across(c(.l, .r), as.character))) |>
      bind_rows()

    diff_count <- sum(object$diffs |> map(\(d) nrow(d)) |> unlist())
    shown <- nrow(shown_diffs)

    shown_diffs |> .print_diffs()
    if (diff_count > shown) .glue("  # ... and {diff_count - shown} more\n") |> cat()
  }

  counts <- discard(object$diffs |> map_int(nrow), \(n) n == 0)
  if (length(counts) > 0) {
    width <- counts |>
      names() |>
      str_length() |>
      max()
    "\ndifference counts by type:\n" |> cat()
    iwalk(counts, \(n, type) sprintf("  %-*s  %d\n", width, type, n) |> cat())
  }

  cat("\n")
  compared <- attr(object, "compared")
  n_rows <- f_num(compared[["rows"]])
  n_cols <- f_num(compared[["cols"]])
  .glue("compared {n_rows} rows and {n_cols} columns\n") |> cat()

  # trailing notes
  cat("\n")
  all_match <- as.logical(object)
  if (all_match) {
    .glue("{cli::symbol$info} all match\n") |> cat()
  }
  dims <- attr(object, "dims")
  if (dims[1] != dims[2]) {
    .glue(
      "{cli::symbol$info} l and r have different lengths",
      " ({f_num(dims[1])} vs {f_num(dims[2])})\n"
    ) |>
      crayon::red() |>
      cat()
  }
  if (!all_match) {
    n_diff <- nrow(object$schema) + nrow(object$keys) +
      sum(map_int(object$diffs, nrow))
    .glue("{cli::symbol$info} {f_num(n_diff)} differences found\n") |>
      crayon::red() |>
      cat()
  }

  invisible(object)
}

#' @rdname summary.kompare
#' @export
print.kompare <- \(x, ...) {
  x |> summary(...)
  invisible(x)
}

#' coerce a kompare object to a logical
#'
#' @param x a `kompare` object, as returned by [kompare()].
#' @param ... not used.
#'
#' @return `TRUE` if `l` and `r` have no schema mismatches, unmatched keys,
#'   or value differences; `FALSE` otherwise.
#'
#' @export
as.logical.kompare <- \(x, ...) {
  nrow(x$schema) == 0 && nrow(x$keys) == 0 && length(x$diffs) == 0
}
