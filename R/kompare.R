# reject names that clash with internal columns
check_names <- \(d, name) {
  if (".rn" %in% names(d)) {
    abort(glue("{name} has a reserved column name: .rn"))
  }
  if (any(str_detect(names(d), "\x1f"))) {
    abort(glue("{name} has a column name containing the unit separator (\\x1f)"))
  }
  invisible(d)
}

# lower-case names in a tidyselect expression so matching ignores case
lower_selection <- \(expr) {
  if (is_symbol(expr)) return(sym(str_to_lower(as_string(expr))))
  if (is_character(expr)) return(str_to_lower(expr))
  if (is_call(expr)) return(as.call(map(as.list(expr), lower_selection)))
  expr
}

# resolve ignore with tidyselect, lowering selections to match column names
resolve_ignore_cols <- \(quo, l, r) {
  cols <- union(names(l), names(r))
  proxy <- set_names(as.list(rep(0L, length(cols))), cols)
  quo <- quo_set_expr(quo, lower_selection(quo_get_expr(quo)))
  tidyselect::eval_select(quo, proxy, allow_rename = FALSE) |> names()
}

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
  check_names(d, name)
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
#' @param strings `"strict"` (default) or a `_`-joined combination of
#'   `"ignorecase"` and `"trim"`, in any order (e.g. `"ignorecase_trim"`).
#' @param factors `"as_strings"` (default) or `"as_factors"` (compared as
#'   factors; columns whose levels differ in set or order land in `schema`).
#' @param raise_error Abort with the printed summary on differences. Default
#'   `FALSE`.
#' @param ignore Columns skipped in value comparisons, selected with
#'   [tidyselect::language] syntax: character vectors, bare names, or helpers
#'   (e.g. `c(price)`, `starts_with("cost")`). Matching ignores case. Columns
#'   must exist in at least one frame. `NULL` (default) ignores nothing.
#' @param fn Applied to both frames before comparing; its output is ungrouped.
#'   Default `\(d) d`.
#' @param ... Aliases.
#'
#' @return A `kompare` list: `schema` (columns missing on one side or
#'   retyped), `keys` (rows with no match), and `diffs` (differing rows, by
#'   type). Prints as a summary and coerces to logical with [as.logical()].
#'
#' @details
#' Names are matched case-insensitively; only columns sharing name and type
#' are compared for values. With `factors = "as_factors"`, factor columns
#' whose levels differ in set or order land in `schema`, with their level
#' definitions shown. `POSIXct` columns at midnight become `Date`, with a
#' message. `by` columns must be unique on each side. `print()` also reports
#' matched rows and compared columns.
#'
#' @section Error conditions:
#' With `raise_error = TRUE`, errors of class `kompare_error` carry the
#' report in `body` and the result in `obj`.
#'
#' @examples
#' l <- tibble::tibble(id = 1:3, x = c(1, 2, 3))
#' r <- tibble::tibble(id = 1:3, x = c(1, 2.5, 3))
#' kompare(l, r, dplyr::join_by(id))
#'
#' @importFrom rlang abort warn inform expr sym arg_match inherits_any is_scalar_integerish is_string is_bool enquo quo_get_expr quo_set_expr is_symbol is_call as_string
#' @importFrom dplyr rename_with select all_of mutate row_number full_join inner_join filter distinct pull rename join_by cur_column ungroup
#' @importFrom tidyr pivot_longer drop_na
#' @importFrom purrr map map_chr map2 set_names discard
#' @importFrom stringr str_to_lower str_c str_trim str_split_1 str_detect
#' @importFrom glue glue
#' @importFrom tibble tibble
#' @importFrom stats na.omit
#' @importFrom lubridate as_date hour minute second
#' @importFrom vctrs vec_ptype_abbr vec_equal
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

  # arg validation and parsing
  if (!is.numeric(digits) || !is_scalar_integerish(digits) || !is.finite(digits)) {
    abort("digits must be a single integer")
  }
  digits <- as.integer(digits)
  if (!is_string(strings)) {
    abort("strings must be a single string")
  }
  if (!is_bool(raise_error)) {
    abort("raise_error must be TRUE or FALSE")
  }

  factors <- arg_match(factors, c("as_strings", "as_factors"))

  sf <- if (!identical(strings, "strict")) {
    str_split_1(strings, "_") |> str_to_lower()
  } else character()
  if (length(setdiff(sf, c("ignorecase", "trim"))) > 0) {
    abort(glue('strings must be "strict" or a combination of ignorecase and trim, not "{strings}"'))
  }
  sf_ignorecase <- "ignorecase" %in% sf
  sf_trim       <- "trim" %in% sf

  # validate data frame inputs
  l <- validate_frame(l, "l")
  r <- validate_frame(r, "r")

  # helper functions
  clean_types <- \(d) {
    if (factors == "as_strings") {
      return(mutate(d, across(where(is.factor), as.character)))
    }
    mutate(d, across(
      all_of(intersect(factor_cols, names(d))),
      \(x) factor(x, levels = factor_lvls[[cur_column()]])
    ))
  }
  all_midnight <- \(x) all(hour(x) == 0 & minute(x) == 0 & second(x) == 0, na.rm = TRUE)
  date_cols <- \(d) d |>
    select(where(\(x) inherits_any(x, "POSIXt") && all_midnight(x))) |>
    names()
  neq <- \(x, y) !vec_equal(x, y, na_equal = TRUE)

  # factor cols shared by both sides, with levels unioned for comparison
  factor_cols <- intersect(
    l |> select(where(is.factor)) |> names(),
    r |> select(where(is.factor)) |> names()
  )
  factor_lvls <- factor_cols |>
    set_names(factor_cols) |>
    map(\(col) union(levels(l[[col]]), levels(r[[col]])))

  # datetime columns with no time component, warned once across both sides
  # coerce to date where needed
  cols_to_date <- union(date_cols(l), date_cols(r))
  if (length(cols_to_date) > 0) {
    inform(glue(
      "columns {str_c(cols_to_date, collapse = ', ')} have no time component and were converted ",
      "to lubridate date"))
  }
  unify_dates <- \(d) mutate(d, across(all_of(intersect(cols_to_date, names(d))), as_date))
  l <- l |> unify_dates()
  r <- r |> unify_dates()

  # construct and validate by
  check_by_repeats <- \(x, side = NULL) {
    if (anyDuplicated(x) > 0) {
      repeats <- x[duplicated(x)] |> unique() |> str_c(collapse = ", ")
      where <- if (is.null(side)) "" else glue(" on the {side} side")
      abort(glue("by repeats a column{where}: {repeats}"))
    }
  }
  check_by_exists <- \(x, cols, side) {
    missing <- setdiff(x, cols)
    if (length(missing) > 0) {
      abort(glue("by columns missing from {side}: {str_c(missing, collapse = ', ')}"))
    }
  }
  if (!is.null(by) && !is.character(by) && !inherits_any(by, "dplyr_join_by")) {
    abort("by must be NULL, a character vector, or a dplyr::join_by() object")
  }
  if (is.character(by)) { # parse by into join_by, if is character
    if (length(by) == 0) {
      abort("by must specify at least one column")
    }
    if (is.null(names(by))) {
      lhs <- rhs <- by
    } else {
      if (any(names(by) == "" | is.na(names(by)))) {
        abort("by names must either all be given or all omitted")
      }
      lhs <- names(by)
      rhs <- unname(by)
    }
    by <- join_by(!!!map2(lhs, rhs, \(x, y) expr(!!sym(x) == !!sym(y))))
  }

  # allow by to be a join_by() object for asymmetric l/r column names,
  # same as left_join(l, r, by); rename r's cols to match l
  if (inherits_any(by, "dplyr_join_by")) {
    if (any(by$condition != "==") || any(by$filter != "none")) {
      conditions <- str_c(unique(by$condition), collapse = ", ")
      abort(glue("by must contain only equality conditions (==), not: {conditions}"))
    }
    by_l <- by$x |> str_to_lower()
    by_r <- by$y |> str_to_lower()
    reserved <- intersect(union(by_l, by_r), c("col", ".l", ".r"))
    if (length(reserved) > 0) {
      abort(glue("by cannot use reserved column names: {str_c(reserved, collapse = ', ')}"))
    }

    check_by_repeats(by_l, "l")
    check_by_repeats(by_r, "r")
    check_by_exists(by_l, names(l), "l")
    check_by_exists(by_r, names(r), "r")

    clash <- intersect(setdiff(by_l, by_r), names(r))
    if (length(clash) > 0) {
      abort(glue(
        "by cannot rename r columns to {str_c(clash, collapse = ', ')}; ",
        "r already has those columns"
      ))
    }

    r <- r |> rename(!!!set_names(by_r, by_l))
    by <- by_l
  }

  # apply user-specified optional function
  if (!is.function(fn)) {
    abort("fn must be a function")
  }
  apply_fn <- \(d, side) {
    out <- fn(d)
    if (!is.data.frame(out)) {
      abort(glue("fn must return a data frame; {side} returned {class(out)[1]}"))
    }
    out <- out |> ungroup()
    check_names(out, side)
    if (!is.null(by)) {
      check_by_exists(by, names(out), side)
    }
    out
  }
  l <- apply_fn(l, "l")
  r <- apply_fn(r, "r")
  n_cols <- c(ncol(l), ncol(r))

  # resolve columns ignored in case insensitive tidyselect
  ignore <- resolve_ignore_cols(enquo(ignore), l, r)

  # assign row number if necessary; check for key duplicates
  has_key_dupes <- \(d) any(d |> select(all_of(by)) |> duplicated())
  format_dupes <- \(d) {
    k <- d |> select(all_of(by))
    k <- k[duplicated(k), , drop = FALSE] |> head(3)
    map_chr(seq_len(nrow(k)), \(i) {
      str_c(unlist(k[i, ], use.names = FALSE), collapse = "/")
    }) |>
      str_c(collapse = ", ")
  }
  if (is.null(by)) {
    l <- l |> mutate(.rn = row_number())
    r <- r |> mutate(.rn = row_number())
    by <- ".rn"
  } else {
    if (has_key_dupes(l)) {
      abort(glue(
        "by {str_c(by, collapse = ', ')} must uniquely identify all rows in l; ",
        "repeated keys: {format_dupes(l)}"
      ))
    }
    if (has_key_dupes(r)) {
      abort(glue(
        "by {str_c(by, collapse = ', ')} must uniquely identify all rows in r; ",
        "repeated keys: {format_dupes(r)}"
      ))
    }
  }

  # find mismatching data types and factors
  lvl_mismatch <- if (factors == "as_factors") {
    l |> select(where(is.factor)) |> names() |>
      intersect(r |> select(where(is.factor)) |> names()) |>
      discard(\(col) identical(levels(l[[col]]), levels(r[[col]])))
  } else character()
  col_type <- \(d, col) {
    tag <- if (col %in% lvl_mismatch) str_c("[", str_c(levels(d[[col]]), collapse = ","), "]") else ""
    str_c(vec_ptype_abbr(d[[col]]), tag)
  }
  dt <- full_join(
    tibble(col = names(l), dt.l = map_chr(names(l), \(col) col_type(l, col))),
    tibble(col = names(r), dt.r = map_chr(names(r), \(col) col_type(r, col))),
    by = "col"
  ) |>
    filter(!col %in% by)

  # warn if data is very large
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
      if (length(cols) == 0) return(tibble())
      comp <- inner_join(
        l |> select(all_of(c(by, cols))) |> clean_types(),
        r |> select(all_of(c(by, cols))) |> clean_types(),
        by = by, suffix = c("\x1f.l", "\x1f.r")
      ) |>
        pivot_longer(-all_of(by), names_sep = "\x1f", names_to = c("col", ".value"))
      if (type %in% c("dbl", "int")) {
        comp |>
          mutate(.l = round(.l, digits), .r = round(.r, digits)) |>
          filter(neq(.l, .r))
      } else if (type == "chr" && (sf_ignorecase || sf_trim)) {
        clean_str <- \(s) {
          if (sf_ignorecase) s <- str_to_lower(s)
          if (sf_trim) s <- str_trim(s)
          s
        }
        comp |>
          mutate(.l = clean_str(.l), .r = clean_str(.r)) |>
          filter(neq(.l, .r))
      } else {
        comp |> filter(neq(.l, .r))
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
      cols = length(compared_cols) + length(setdiff(by, ignore))
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
      obj = summ,
      body = .format_kompare(summ) |> str_c(collapse = "\n"),
      call = NULL
    )
  }
  return(summ)
}
