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

# one display string per row, whatever the column type
.fmt_cell <- \(x) {
  if (is.matrix(x) || is.data.frame(x)) return(
    map_chr(seq_len(nrow(x)), \(i) str_c(unlist(x[i,], use.names = FALSE), collapse = "/"))
  )
  if (is.list(x)) return(map_chr(x, \(v) str_c(format(v), collapse = "/")))
  as.character(x)
}

# positive whole number of rows to show
.check_n <- \(n) {
  if (!is_scalar_integerish(n) || !is.finite(n) || n < 1) {
    abort("n must be a positive integer")
  }
  as.integer(n)
}

# first unused column name for the diff type label
.type_label <- \(d) {
  if (!"type" %in% names(d)) return("type")
  label <- "data_type"
  while (label %in% names(d)) label <- str_c(".", label)
  label
}

# number of differences in a kompare object
.n_diffs <- \(x) nrow(x$schema) + nrow(x$keys) + sum(map_int(x$diffs, nrow))

# render a kompare object as lines
.format_kompare <- \(x, n = 5) {
  n <- .check_n(n)
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
      imap(\(d, type) {
        label <- .type_label(d)
        d |>
          head(n) |>
          mutate(!!sym(label) := .env$type, .before = 1) |>
          mutate(.l = .fmt_cell(.l), .r = .fmt_cell(.r))
      }) |>
      bind_rows()
    lines <- c(lines, .table_lines(shown_diffs, nrow(shown_diffs)))
    diff_count <- sum(map_int(x$diffs, nrow))
    if (diff_count > nrow(shown_diffs)) {
      lines <- c(lines, cli::col_grey(.glue("  # ... and {f_num(diff_count - nrow(shown_diffs))} more")))
    }
  }

  lines <- c(lines, "", .glue(
    "compared {f_num(compared[['rows']])} rows and {f_num(compared[['cols']])} columns"
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
#' @param object,x A `kompare` object.
#' @param n Number of rows to show.
#' @param ... Not used.
#'
#' @return `object`, invisibly.
#'
#' @importFrom dplyr bind_rows
#' @importFrom glue glue
#' @importFrom stringr str_c str_detect str_remove_all
#' @importFrom purrr imap map_int
#' @importFrom utils head capture.output
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
#' @return A tibble of `type`, `col`, `diffs`, and `n` (observations
#'   compared).
#'
#' @importFrom dplyr bind_rows
#' @importFrom generics tidy
#'
#' @export
tidy.kompare <- \(x, ...) {
  n <- as.integer(attr(x, "compared")[["rows"]])
  x$diffs |>
    imap(\(d, type) {
      tab <- table(d[["col"]])
      tibble::tibble(type = type, col = names(tab), diffs = as.integer(tab), n = n)
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

#' Do two frames match
#'
#' @param x A `kompare` object.
#'
#' @return `TRUE` when there are no schema mismatches, unmatched keys, or
#'   value differences.
#'
#' @export
is_equal <- \(x) as.logical(x)
