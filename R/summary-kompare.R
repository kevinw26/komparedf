# glue variant that keeps whitespace and sees the caller's variables
.glue <- \(...) glue(..., .trim = FALSE, .envir = parent.frame())

# count with the locale thousands separator
f_num <- \(x) {
  format(x, big.mark = Sys.localeconv()[["thousands_sep"]], scientific = FALSE)
}

# an indented table, minus tibble class and type rows
.table_lines <- \(d, n = 5) {
  lines <- capture.output(print(head(d, n)))
  lines <- lines[!str_detect(str_remove_all(lines, "\x1b\\[[0-9;]*m"), "^\\s*(<|#)")]
  .glue("  {lines}")
}

# number of differences in a kompare object
.n_diffs <- \(x) nrow(x$schema) + nrow(x$keys) + sum(map_int(x$diffs, nrow))

# render a kompare object as lines
.format_kompare <- \(x, n = 5) {
  settings <- attr(x, "settings")
  dims <- attr(x, "dims")
  compared <- attr(x, "compared")
  by_label <- if (identical(settings$by, ".rn")) "position" else str_c(settings$by, collapse = ", ")

  lines <- cli::col_grey(.glue(
    "# kompare: {f_num(dims[1])} x {f_num(settings$n_cols[1])}",
    " vs {f_num(dims[2])} x {f_num(settings$n_cols[2])}, by: {by_label}"
  ))

  shown <- c(
    if (settings$digits != 4) .glue("digits = {settings$digits}"),
    if (settings$strings != "strict") .glue("strings = {settings$strings}"),
    if (settings$factors != "as_strings") .glue("factors = {settings$factors}"),
    if (length(settings$ignore) > 0) .glue("ignore = {str_c(settings$ignore, collapse = ', ')}")
  )
  if (length(shown) > 0) {
    lines <- c(lines, cli::col_grey(.glue("# settings: {str_c(shown, collapse = ', ')}")))
  }

  lines <- c(lines, .glue("schema mismatches: {f_num(nrow(x$schema))}"))
  if (nrow(x$schema) > 0) {
    lines <- c(lines, .table_lines(x$schema, n))
    if (nrow(x$schema) > n) {
      lines <- c(lines, cli::col_grey(.glue("  # ... and {f_num(nrow(x$schema) - n)} more")))
    }
    lines <- c(lines, "")
  }

  lines <- c(lines, .glue("unmatched keys: {f_num(nrow(x$keys))}"))
  if (nrow(x$keys) > 0) {
    n_l <- sum(is.na(x$keys$.r))
    n_r <- sum(is.na(x$keys$.l))
    lines[length(lines)] <- .glue(
      "unmatched keys: {f_num(nrow(x$keys))}",
      " ({n_l} in l only, {n_r} in r only)"
    )
    lines <- c(lines, .table_lines(x$keys, n))
    if (nrow(x$keys) > n) {
      lines <- c(lines, cli::col_grey(.glue("  # ... and {f_num(nrow(x$keys) - n)} more")))
    }
    lines <- c(lines, "")
  }

  if (length(x$diffs) == 0) {
    lines <- c(lines, "value differences: 0")
  } else {
    lines <- c(lines, "value differences by type:")
    shown_diffs <- x$diffs |>
      imap(\(d, type) d |>
        head(n) |>
        mutate(type = type, .before = 1) |>
        mutate(across(c(.l, .r), as.character))) |>
      bind_rows()
    lines <- c(lines, .table_lines(shown_diffs, nrow(shown_diffs)))
    diff_count <- sum(map_int(x$diffs, nrow))
    if (diff_count > nrow(shown_diffs)) {
      lines <- c(lines, cli::col_grey(.glue("  # ... and {f_num(diff_count - nrow(shown_diffs))} more")))
    }
  }

  lines <- c(lines, "", .glue(
    "compared {f_num(compared[['rows']])} rows",
    " and {f_num(compared[['cols']])} columns"
  ))

  notes <- character()
  if (is_equal(x)) {
    notes <- c(notes, cli::col_green(.glue("{cli::symbol$info} all match")))
  }
  if (!is_equal(x)) {
    notes <- c(notes, cli::col_red(.glue(
      "{cli::symbol$info} {f_num(.n_diffs(x))} differences found"
    )))
  }
  if (dims[1] != dims[2]) {
    notes <- c(notes, cli::col_red(.glue(
      "{cli::symbol$info} l and r have different lengths",
      " ({f_num(dims[1])} vs {f_num(dims[2])})"
    )))
  }
  if (length(notes) > 0) lines <- c(lines, "", notes)

  lines
}

#' Summarize a kompare object
#'
#' Prints the difference report.
#'
#' @param object A `kompare` object.
#' @param ... Not used.
#'
#' @return `object`, invisibly.
#'
#' @importFrom dplyr bind_rows
#' @importFrom glue glue
#' @importFrom stringr str_c str_detect str_remove_all
#' @importFrom purrr imap map_int
#'
#' @export
summary.kompare <- \(object, n = 5, ...) {
  writeLines(format(object, n = n))
  invisible(object)
}

#' @rdname summary.kompare
#' @export
print.kompare <- \(x, n = 5, ...) {
  summary(x, n = n)
  invisible(x)
}

#' @rdname summary.kompare
#' @export
format.kompare <- \(x, n = 5, ...) {
  .format_kompare(x, n)
}

#' Tidy differences
#'
#' One row per column with value differences.
#'
#' @param x A `kompare` object.
#' @param ... Not used.
#'
#' @return A tibble of `type`, `col`, and `diffs`.
#'
#' @importFrom dplyr bind_rows
#' @importFrom generics tidy
#'
#' @export
tidy.kompare <- \(x, ...) {
  x$diffs |>
    imap(\(d, type) {
      tab <- table(d[["col"]])
      tibble::tibble(type = type, col = names(tab), diffs = as.integer(tab))
    }) |>
    bind_rows()
}

#' Coerce a kompare object to a logical
#'
#' @param x A `kompare` object.
#' @param ... Not used.
#'
#' @return `TRUE` if nothing differs.
#'
#' @export
as.logical.kompare <- \(x, ...) {
  nrow(x$schema) == 0 && nrow(x$keys) == 0 && length(x$diffs) == 0
}
