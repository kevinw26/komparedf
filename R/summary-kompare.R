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
  lines <- str_trim(lines, side = "right")
  .glue("  {lines}")
}

# pillar-rendered text; integer64 needs its own exact formatter
.shaft <- \(x) {
  if (inherits(x, "integer64")) return(str_trim(format(x)))
  str_trim(format(pillar_shaft(x), width = 1000))
}

# pillar text for one cell: scalars plain, wider cells clipped to six values
.cell_values <- \(v) {
  if (!length(v)) return("")
  clipped <- length(v) > 6
  s <- .shaft(if (clipped) v[1:6] else v)
  if (length(s) == 1) return(s)
  str_c("<", str_c(s, collapse = ", "), if (clipped) ", …", ">")
}

# cell text via pillar, so every row binds as one character column
.cell <- \(x) {
  if (length(dim(x)) > 1) {
    x <- map(seq_len(nrow(x)), \(i) {
      v <- vec_slice(x, i)
      if (is.data.frame(v)) unlist(v, use.names = FALSE) else as.vector(v)
    })
  }
  if (!is.list(x)) return(.shaft(x))
  map_chr(x, .cell_values)
}

# positive whole number of rows to show
.check_n <- \(n) {
  if (!is_scalar_integerish(n) || !is.finite(n) || n < 1) {
    abort("n must be a positive integer")
  }
  as.integer(n)
}

# format plain doubles; classed doubles need their own pillar methods
.display <- \(v, digits) {
  if (is.null(digits) || !is.double(v) || is.object(v)) return(v)
  pillar::num(v, digits = digits)
}

# first unused column name for the diff type label
.type_label <- \(d) {
  if (!"type" %in% names(d)) return("type")
  label <- "data_type"
  while (label %in% names(d)) label <- str_c(".", label)
  label
}

# number of differences in a kompare object
.n_diffs <- \(x) nrow(x$schema) +
  nrow(x$keys) +
  sum(map_int(x$diffs, nrow))

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
    if (!identical(settings$digits, 4)) {
      if (is.null(settings$digits)) "no rounding" else .glue("digits = {settings$digits}")
    },
    if (settings$strings != "strict") .glue("strings = {settings$strings}"),
    if (settings$factors != "as_strings") .glue("factors = {settings$factors}")
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
          mutate(
            .l = .cell(.display(.l, settings$digits)),
            .r = .cell(.display(.r, settings$digits))
          )
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
  if (as.logical(x)) {
    notes <- c(notes, cli::col_green(.glue("{cli::symbol$info} all match")))
  }
  if (!as.logical(x)) {
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
#' @importFrom pillar pillar_shaft
#' @importFrom stringr str_c str_detect str_remove_all
#' @importFrom purrr imap map_int
#' @importFrom utils head capture.output
#' @importFrom vctrs vec_slice
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
#' @importFrom tibble tibble
#'
#' @export
tidy.kompare <- \(x, ...) {
  n <- as.integer(attr(x, "compared")[["rows"]])
  if (sum(map_int(x$diffs, nrow)) == 0) {
    return(tibble(type = character(), col = character(), diffs = integer(), n = integer()))
  }

  x$diffs |>
    imap(\(d, type) {
      tab <- table(d[["col"]])
      tibble(type = type, col = names(tab), diffs = as.integer(tab), n = n)
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
as.logical.kompare <- \(x, ...) .n_diffs(x) == 0
