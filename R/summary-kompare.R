.glue <- \(...) glue(..., .trim = FALSE, .envir = parent.frame())

.print_diffs <- \(d) {
  lines <- d |>
    format() |>
    tail(-1)
  lines |>
    discard(lines |> str_remove_all("\x1b\\[[0-9;]*m") |> str_detect("^\\s*<")) |>
    str_c("  ", ... = _) |>
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
#' @importFrom dplyr count pull
#' @importFrom stringr str_c
#' @importFrom purrr iwalk
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
  }

  # value differences: show the first five columns per type, first five
  # rows of differences for those columns, printed with the tibble printer
  if (length(object$diffs) == 0) {
    "value differences: 0\n" |> cat()
  } else {
    "\nvalue differences by type (up to three of each type shown):\n" |> cat()
    shown_diffs <- object$diffs |>
      imap(\(d, type) d |>
        head(3) |>
        mutate(type = type, .before = 1) %>%
        mutate(across(c(.l, .r), as.character))) |>
      bind_rows()

    diff_count <- sum(out$diffs |> map(\(d) nrow(d)) |> unlist())
    shown <- nrow(shown_diffs)

    shown_diffs |> .print_diffs()
    if (diff_count > shown) .glue("  # ... and {diff_count - shown} more\n") |> cat()
  }

  counts <- discard(object$diffs |> map_int(nrow), \(n) n == 0)
  if (length(counts) == 0) {
    return(invisible())
  }

  width <- counts |>
    names() |>
    str_length() |>
    max()
  "\ndifference counts by type:\n" |> cat()
  iwalk(counts, \(n, type) sprintf("  %-*s  %d\n", width, type, n) |> cat())

  # note if l and r have different row counts, since this affects how
  # meaningful the key/diff comparisons are
  cat("\n")
  dims <- attr(object, "dims")
  if (dims[1] != dims[2]) {
    .glue("{cli::symbol$info} l and r have different lengths ({dims[1]} vs {dims[2]})\n") |>
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
