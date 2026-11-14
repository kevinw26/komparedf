# check a frame, lower-case its names, reject duplicates the lowercasing creates
validate_frame <- \(d, name) {
  if (!is.data.frame(d)) {
    abort(glue("{name} must be a data frame, not {class(d)[1]}"))
  }
  d <- d |> set_names(str_to_lower(names(d)))
  dupes <- names(d)[duplicated(names(d))]
  if (length(dupes) > 0) {
    abort(glue("{name} has duplicate column names after lowercasing: {str_c(unique(dupes), collapse = ', ')}"))
  }
  d
}

# warn when a comparison will touch roughly more cells than limit
check_cells <- \(l, r, cols, limit = 10e6) {
  cells <- max(nrow(l), nrow(r)) * length(cols)
  if (cells > limit) {
    warn(glue("about {format(cells, big.mark = ',', scientific = FALSE)} cells to compare, may be slow"))
  }
  invisible(cells)
}

#' Compare two data frames
#'
#' Compares two data frames for differences in schema (column names/types),
#' keys (rows present in one but not the other), and values. Column names
#' are matched case-insensitively, and compatible date/datetime columns are
#' unified before comparison.
#'
#' @param l A data frame (the "left" side of the comparison).
#' @param r A data frame (the "right" side of the comparison).
#' @param by How to match rows between `l` and `r`, as a [dplyr::join_by()]
#'   object (eg `join_by(id)` or `join_by(l_id == r_id)`), a character
#'   vector of column names that uniquely identify a row in both, or a named
#'   character vector for asymmetric names (names are `l` columns, values are
#'   `r` columns, eg `c(l_id = "r_id")`). Matched the same way as
#'   `left_join(l, r, by)`. If `NULL` (the default), rows are compared by
#'   position instead, which assumes both data frames are already in the same
#'   row order.
#' @param .round Number of decimal places of tolerance to use when comparing
#'   numeric/integer columns. Differences smaller than `10^-.round` are
#'   treated as equal. Default is `4`. `roundDigits` and `round` are accepted
#'   as aliases.
#' @param .strings How to compare character columns. `"strict"` (the default)
#'   compares values exactly; `"loose"` compares after lowercasing and
#'   trimming whitespace, so case and leading/trailing spaces are ignored.
#' @param .factor How to compare factor columns. `"as_strings"` (the default)
#'   compares factor values as character; `"as_factors"` harmonises levels
#'   across `l` and `r` (union of both) and compares values as factors.
#' @param .raise_error If `TRUE`, print a summary and throw an error when
#'   any differences are found (schema mismatches, unmatched keys, or value
#'   differences). Default is `FALSE`.
#' @param .ignore Character vector of column names to exclude from value
#'   comparison entirely (eg columns expected to always differ, like an
#'   `updated_at` timestamp). Excluded columns can still appear in `schema`
#'   if their types mismatch.
#' @param .fn A function to apply to both data frames before comparing
#'   (eg [dplyr::arrange_all()]). Executes after column names are forced to
#'   lower case and renaming, if any, by `by`.
#'
#' @return A `kompare` object: a list with three elements, printable via
#'   [print.kompare()]/[summary.kompare()] and coercible to a logical via
#'   [as.logical.kompare()].
#' \describe{
#'   \item{`schema`}{A data frame of columns whose type differs between `l`
#'     and `r`, or that exist in only one of the two data frames.}
#'   \item{`keys`}{A data frame of key values that exist in only one of `l`
#'     or `r` (i.e. rows that couldn't be matched for comparison).}
#'   \item{`diffs`}{A named list, one element per shared data type (eg
#'     `"numeric"`, `"character"`), each containing a data frame of rows
#'     where values differ between `l` and `r` for columns of that type.
#'     Numeric columns are compared with tolerance `.round`; character
#'     columns are compared exactly unless `.strings = "loose"`; all
#'     other types are compared for exact equality. In every case, a value
#'     that is `NA` on one side and non-`NA` on the other also counts as a
#'     difference. Types with zero differences are omitted.}
#' }
#'
#' @details
#' Only columns that share the same name (case-insensitive) and the same
#' data type in both `l` and `r` are compared for value differences; columns
#' that differ in type, or exist in only one data frame, are reported in
#' `schema` instead and excluded from `diffs`. Factor columns are reported
#' under their original type in `schema`.
#' `POSIXct`/`POSIXlt` columns with no time-of-day component (i.e. every
#' non-`NA` value falls at midnight) are converted to `Date` before
#' comparison, with a warning identifying the affected column.
#'
#' If `by` is supplied, both `l` and `r` must have no duplicate rows with
#' respect to those columns, or `kompare()` will error.
#'
#' @examples
#' library(dplyr)
#' l <- data.frame(id = 1:3, x = c(1, 2, 3), y = c("a", "b", "c"))
#' r <- data.frame(id = 1:3, x = c(1, 2.5, 3), y = c("a", "B", "z"))
#'
#' result <- kompare(l, r, join_by(id))
#' result$schema
#' result$keys
#' result$diffs
#'
#' # without by, rows are compared by position
#' kompare(l, r)
#'
#' # ignore whitespace/case differences in strings
#' kompare(l, r, join_by(id), .strings = "loose")
#'
#' # error immediately if any differences are found
#' \dontrun{
#' kompare(l, r, join_by(id), .raise_error = TRUE)
#' }
#'
#' @importFrom rlang abort warn expr sym arg_match inherits_any
#' @importFrom dplyr rename_with select all_of mutate row_number full_join inner_join filter distinct pull rename join_by cur_column
#' @importFrom tidyr pivot_longer drop_na
#' @importFrom purrr map map_chr map2 set_names discard
#' @importFrom stringr str_to_lower str_c str_trim
#' @importFrom glue glue
#' @importFrom tibble tibble
#' @importFrom stats na.omit
#' @importFrom lubridate as_date hour minute second
#'
#' @export
kompare <- \(
  l, r, by = NULL, .round = 4, .strings = "strict", .factor = "as_strings",
  .raise_error = FALSE, .ignore = c(), .fn = \(d) d, ...
) {
  # aliases from dots: keys -> by; roundDigits, round -> .round
  dots <- list(...)
  if (!is.null(dots$keys)) by <- dots$keys
  if (!is.null(dots$roundDigits)) .round <- dots$roundDigits
  if (!is.null(dots$round)) .round <- dots$round

  # enum args
  .strings <- arg_match(.strings, c("strict", "loose"))
  .factor <- arg_match(.factor, c("as_strings", "as_factors"))

  # validate inputs and normalise column names
  l <- validate_frame(l, "l")
  r <- validate_frame(r, "r")

  # helper functions
  has_dupes <- \(d) any(d |> select(all_of(by)) |> duplicated())

  # repeated key combinations, shown in the duplicate key error
  repeated_keys <- \(d) {
    k <- d |> select(all_of(by))
    k <- k[duplicated(k), , drop = FALSE]
    k |>
      head(3) |>
      apply(1, \(row) str_c(as.character(row), collapse = "/")) |>
      str_c(collapse = ", ")
  }

  # factor cols shared by both sides, with levels unioned for comparison
  factor_cols <- intersect(
    l |> select(where(is.factor)) |> names(),
    r |> select(where(is.factor)) |> names()
  )
  factor_lvls <- factor_cols |>
    set_names(factor_cols) |>
    map(\(col) union(levels(l[[col]]), levels(r[[col]])))
  clean_types <- \(d) {
    if (.factor == "as_strings") {
      return(mutate(d, across(where(is.factor), as.character)))
    }
    mutate(d, across(
      all_of(intersect(factor_cols, names(d))),
      \(x) factor(x, levels = factor_lvls[[cur_column()]])
    ))
  }

  # datetime columns with no time component, warned once across both sides
  all_midnight <- \(x) all(hour(x) == 0 & minute(x) == 0 & second(x) == 0, na.rm = TRUE)
  date_cols <- \(d) d |>
    select(where(\(x) inherits_any(x, "POSIXt") && all_midnight(x))) |>
    names()
  cols_to_date <- union(date_cols(l), date_cols(r))
  if (length(cols_to_date) > 0) {
    warn(glue(
      "columns {str_c(cols_to_date, collapse = ', ')} have no time component and were converted ",
      "to lubridate date"))
  }
  unify_dates <- \(d) mutate(d, across(all_of(intersect(cols_to_date, names(d))), as_date))

  # coerce dates, apply the function
  l <- l |> unify_dates()
  r <- r |> unify_dates()

  # abort if a key column is named twice
  no_repeats <- \(x, side = NULL) {
    if (anyDuplicated(x) > 0) {
      repeats <- x[duplicated(x)] |> unique() |> str_c(collapse = ", ")
      where <- if (is.null(side)) "" else glue(" on the {side} side")
      abort(glue("by repeats a column{where}: {repeats}"))
    }
  }

  # named vector: names are l cols, values r cols
  if (is.character(by) && !is.null(names(by))) {
    by <- join_by(!!!map2(names(by), by, \(x, y) expr(!!sym(x) == !!sym(y))))
  }

  # allow by to be a join_by() object for asymmetric l/r column names,
  # same as left_join(l, r, by); rename r's cols to match l
  if (inherits_any(by, "dplyr_join_by")) {
    by$x <- by$x |> str_to_lower()
    by$y <- by$y |> str_to_lower()
    no_repeats(by$x, "l")
    no_repeats(by$y, "r")
    r <- r |> rename(!!!set_names(by$y, by$x))
    by <- by$x
  } else if (!is.null(by)) {
    by <- by |> str_to_lower()
    no_repeats(by)
  }

  # apply optional function
  l <- l |> .fn()
  r <- r |> .fn()

  # assign row number if necessary; check for key duplicates
  if (is.null(by)) {
    l <- l |> mutate(.rn = row_number())
    r <- r |> mutate(.rn = row_number())
    by <- ".rn"
  } else {
    if (has_dupes(l)) {
      abort(glue(
        "by {str_c(by, collapse = ', ')} must uniquely identify all rows in l",
        "; repeated keys: {repeated_keys(l)}"
      ))
    }
    if (has_dupes(r)) {
      abort(glue(
        "by {str_c(by, collapse = ', ')} must uniquely identify all rows in r",
        "; repeated keys: {repeated_keys(r)}"
      ))
    }
  }
  dt <- full_join(
    tibble(col = names(l), dt.l = map_chr(l, \(x) class(x)[1])),
    tibble(col = names(r), dt.r = map_chr(r, \(x) class(x)[1])),
    by = "col"
  ) |>
    filter(!col %in% by)

  # rough cell count from row and column numbers, not the joined data
  compared_cols <- dt |> filter(dt.l == dt.r) |> pull(col) |> setdiff(.ignore)
  check_cells(l, r, compared_cols)

  # construct diffs by type
  d_types <- na.omit(dt |> filter(dt.l == dt.r) |> distinct(dt.l) |> drop_na() |> pull())
  diffs <- d_types |>
    map(\(type) {
      cols <- na.omit(
        dt |> filter(dt.l == type & dt.r == type) |> pull(col) |>
          setdiff(by) |> setdiff(.ignore)
      )
      comp <- inner_join(
        l |> select(all_of(c(by, cols))) |> clean_types(),
        r |> select(all_of(c(by, cols))) |> clean_types(),
        by = by, suffix = c("\x1f.l", "\x1f.r")
      ) |>
        pivot_longer(-all_of(by), names_sep = "\x1f", names_to = c("col", ".value"))
      if (type %in% c("numeric", "integer")) {
        comp |>
          mutate(.l = round(.l, .round), .r = round(.r, .round)) |>
          filter(.l != .r | is.na(.l) != is.na(.r))
      } else if (type == "character" && .strings == "loose") {
        clean_str <- \(s) s |>
          str_to_lower() |>
          str_trim()
        comp |>
          mutate(.l = clean_str(.l), .r = clean_str(.r)) |>
          filter(.l != .r | is.na(.l) != is.na(.r))
      } else {
        comp |> filter(.l != .r | is.na(.l) != is.na(.r))
      }
    }) |>
    set_names(d_types) |>
    discard(\(d) nrow(d) == 0)

  # rows present on only one side
  unmatched <- full_join(
    l |> select(all_of(by)) |> mutate(.l = TRUE),
    r |> select(all_of(by)) |> mutate(.r = TRUE),
    by = by
  ) |>
    filter(is.na(.l) | is.na(.r))

  # make summary
  summ <- structure(
    list(
      "schema" = dt |> filter(is.na(dt.l) | is.na(dt.r) | dt.l != dt.r),
      "keys" = unmatched,
      "diffs" = diffs
    ),
    dims = c(nrow(l), nrow(r)),
    compared = c(
      rows = (nrow(l) + nrow(r) - nrow(unmatched)) / 2,
      cols = length(compared_cols) + length(by)
    ),
    class = "kompare"
  )

  if (!as.logical(summ) && .raise_error) {
    msg <- capture.output(summary(summ)) |> str_c(collapse = "\n")
    abort(str_c("kompare found differences between l and r\n\n", msg), call = NULL)
  }
  return(summ)
}
