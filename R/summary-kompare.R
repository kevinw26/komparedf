# glue variant that keeps whitespace and sees the caller's variables
.glue <- \(...) glue(..., .trim = FALSE, .envir = parent.frame())

# count with the locale thousands separator
f_num <- \(x) {
  format(x, big.mark = Sys.localeconv()[["thousands_sep"]], scientific = FALSE)
}

# an indented table, minus tibble class and type rows
.table_lines <- \(d, n = 5) {
  lines <- d |> head(n) |> print() |> capture.output()
  lines <- lines |>
    keep(\(l) {
      l |>
        str_remove_all("\x1b\\[[0-9;]*m") |>
        str_detect("^\\s*(<|#)", negate = TRUE)
    })
  lines <- str_trim(lines, side = "right")
  .glue("  {lines}")
}

# pillar-rendered text; integer64 needs its own exact formatter
.shaft <- \(x) {
  if (inherits(x, "integer64")) return(x |> format() |> str_trim())
  x |>
    pillar_shaft() |>
    format(width = 1000) |>
    str_trim()
}

# pillar text for one cell: scalars plain, wider cells clipped to six values
.cell_values <- \(v) {
  if (!length(v)) return("")
  clipped <- length(v) > 6
  s <- .shaft(if (clipped) v[1:6] else v)
  if (length(s) == 1) return(s)
  str_c("<", str_c(s, collapse = ", "), if (clipped) ", \u2026", ">")
}

# cell text via pillar, so every row binds as one character column
.cell <- \(x) {
  if (length(dim(x)) > 1) {
    x <- seq_len(x |> nrow()) |>
      map(\(i) {
        v <- vec_slice(x, i)
        if (is.data.frame(v)) unlist(v, use.names = FALSE) else as.vector(v)
      })
  }
  if (!is.list(x)) return(.shaft(x))
  map_chr(x, .cell_values)
}

# format plain doubles; classed doubles (eg date; nanotime) need their own pillar methods
.display <- \(v, digits) {
  if (is.null(digits) || !is.double(v) || is.object(v)) return(v)
  pillar::num(v, digits = digits)
}

# first unused column name for the diff type label
.type_label <- \(d) {
  if (!"type" %in% names(d)) return("type")
  label <- "data_type"
  while (label %in% names(d))
    label <- str_c(".", label)
  label
}

# number of differences in a kompare object
.n_diffs <- \(x) nrow(x$schema) +
  nrow(x$keys) +
  (x$diffs |> map_int(nrow) |> sum())

# render a kompare object as lines
.format_kompare <- \(x, n = 5) {
  n <- check_int(n, cap = TRUE)
  settings <- attr(x, "settings")
  dims <- attr(x, "dims")
  compared <- attr(x, "compared")
  by_label <- if (identical(settings$by, ".rn")) "position" else str_c(settings$by, collapse = ", ")

  lines <- cli::col_grey(.glue(
    "# kompare: {f_num(dims[1])} x {f_num(settings$n_cols[1])}",
    " vs {f_num(dims[2])} x {f_num(settings$n_cols[2])}, by: {by_label}"
  ))

  shown <- c(
    if (is.null(settings$digits)) "no rounding" else .glue("digits = {settings$digits}"),
    if (settings$strings != "strict") .glue("strings = {settings$strings}"),
    if (settings$factors != "as_strings") .glue("factors = {settings$factors}")
  )
  if (length(shown) > 0) {
    lines <- c(lines, cli::col_grey(.glue("# settings: {str_c(shown, collapse = ', ')}")))
  }

  lines <- c(lines, .glue("schema mismatches: {x$schema |> nrow() |> f_num()}"))
  if (nrow(x$schema) > 0) {
    lines <- c(lines, .table_lines(x$schema, n))
    if (nrow(x$schema) > n) {
      lines <- c(lines, cli::col_grey(.glue("  # ... and {f_num(nrow(x$schema) - n)} more")))
    }
    lines <- c(lines, "")
  }

  lines <- c(lines, .glue("unmatched keys: {x$keys |> nrow() |> f_num()}"))
  if (nrow(x$keys) > 0) {
    n_l <- sum(is.na(x$keys$.r))
    n_r <- sum(is.na(x$keys$.l))
    lines[length(lines)] <- .glue(
      "unmatched keys: {x$keys |> nrow() |> f_num()}",
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
            .l = .l |> .display(settings$digits) |> .cell(),
            .r = .r |> .display(settings$digits) |> .cell()
          )
      }) |>
      bind_rows()
    lines <- c(lines, .table_lines(shown_diffs, nrow(shown_diffs)))
    diff_count <- x$diffs |> map_int(nrow) |> sum()
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
  object |> format(n = n) |> writeLines()
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
  n <- attr(x, "compared")[["rows"]] |> as.integer()
  if ((x$diffs |> map_int(nrow) |> sum()) == 0) {
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
