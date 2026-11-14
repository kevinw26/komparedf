# check a frame, lower-case its names, reject duplicates the lowercasing creates
validate_frame <- \(d, name) {
  if (!is.data.frame(d)) {
    abort(glue("{name} must be a data frame, not {class(d)[1]}"))
  }
  d <- d |>
    ungroup() |>
    set_names(str_to_lower(names(d)))
  dupes <- names(d)[duplicated(names(d))]
  if (length(dupes) > 0) {
    cols <- str_c(unique(dupes), collapse = ", ")
    abort(glue(
      "{name} has duplicate column names after lowercasing: {cols}",
      name = name, cols = cols
    ))
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
#' Reports schema, key, and value differences between `l` and `r`.
#'
#' @param l,r Data frames to compare.
#' @param by How to match rows: a [dplyr::join_by()] object, a character
#'   vector of shared column names, or a named vector (`c(l = "r")`). `NULL`
#'   (default) compares rows by position.
#' @param digits Decimal places for numeric tolerance. Default `4`.
#' @param strings `"strict"` (default) or `"loose"` (lowercase and trim
#'   before comparing).
#' @param factors `"as_strings"` (default) or `"as_factors"` (levels unioned
#'   across both sides before comparing).
#' @param raise_error Abort with the printed summary on differences. Default
#'   `FALSE`.
#' @param ignore Columns skipped in value comparisons. Default `NULL`.
#' @param fn Applied to both frames before comparing. Default `\(d) d`.
#' @param ... Aliases.
#'
#' @return A `kompare` list: `schema` (columns missing on one side or
#'   retyped), `keys` (rows with no match), and `diffs` (differing rows, by
#'   type). Prints as a summary and coerces to logical with [as.logical()].
#'
#' @details
#' Names are matched case-insensitively; only columns sharing name and type
#' are compared for values. `POSIXct` columns at midnight become `Date`, with
#' a message. `by` columns must be unique on each side. `print()` also
#' reports matched rows and compared columns.
#'
#' @examples
#' l <- tibble::tibble(id = 1:3, x = c(1, 2, 3))
#' r <- tibble::tibble(id = 1:3, x = c(1, 2.5, 3))
#' kompare(l, r, dplyr::join_by(id))
#'
#' @importFrom rlang abort warn inform expr sym arg_match inherits_any
#' @importFrom dplyr rename_with select all_of mutate row_number full_join inner_join filter distinct pull rename join_by cur_column ungroup
#' @importFrom tidyr pivot_longer drop_na
#' @importFrom purrr map map_chr map2 set_names discard
#' @importFrom stringr str_to_lower str_c str_trim
#' @importFrom glue glue
#' @importFrom tibble tibble
#' @importFrom stats na.omit
#' @importFrom lubridate as_date hour minute second
#' @importFrom vctrs vec_ptype_abbr
#'
#' @export
kompare <- \(
  l, r, by = NULL, digits = 4, strings = "strict", factors = "as_strings",
  raise_error = FALSE, ignore = NULL, fn = \(d) d, ...
) {
  # aliases from dots: keys -> by; roundDigits, round -> digits
  dots <- list(...)
  if (!is.null(dots$keys)) by <- dots$keys
  if (!is.null(dots$roundDigits)) digits <- dots$roundDigits
  if (!is.null(dots$round)) digits <- dots$round

  # enum args
  strings <- arg_match(strings, c("strict", "loose"))
  factors <- arg_match(factors, c("as_strings", "as_factors"))

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
    if (factors == "as_strings") {
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
    inform(glue(
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
  l <- l |> fn()
  r <- r |> fn()
  n_cols <- c(ncol(l), ncol(r))

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
    tibble(col = names(l), dt.l = map_chr(l, \(x) vec_ptype_abbr(x))),
    tibble(col = names(r), dt.r = map_chr(r, \(x) vec_ptype_abbr(x))),
    by = "col"
  ) |>
    filter(!col %in% by)

  # rough cell count from row and column numbers, not the joined data
  compared_cols <- dt |> filter(dt.l == dt.r) |> pull(col) |> setdiff(ignore)
  check_cells(l, r, compared_cols)

  # construct diffs by type
  d_types <- na.omit(dt |> filter(dt.l == dt.r) |> distinct(dt.l) |> drop_na() |> pull())
  diffs <- d_types |>
    map(\(type) {
      cols <- na.omit(
        dt |> filter(dt.l == type & dt.r == type) |> pull(col) |>
          setdiff(by) |> setdiff(ignore)
      )
      comp <- inner_join(
        l |> select(all_of(c(by, cols))) |> clean_types(),
        r |> select(all_of(c(by, cols))) |> clean_types(),
        by = by, suffix = c("\x1f.l", "\x1f.r")
      ) |>
        pivot_longer(-all_of(by), names_sep = "\x1f", names_to = c("col", ".value"))
      if (type %in% c("dbl", "int")) {
        comp |>
          mutate(.l = round(.l, digits), .r = round(.r, digits)) |>
          filter(.l != .r | is.na(.l) != is.na(.r))
      } else if (type == "chr" && strings == "loose") {
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
    settings = list(
      by = by,
      digits = digits,
      strings = strings,
      factors = factors,
      ignore = ignore,
      n_cols = n_cols
    ),
    class = "kompare"
  )

  if (!is_equal(summ) && raise_error) {
    abort(
      "kompare found differences between l and r",
      class = "kompare_error",
      summary = summ,
      body = .format_kompare(summ) |> str_c(collapse = "\n"),
      call = NULL
    )
  }
  return(summ)
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
