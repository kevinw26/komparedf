# names used in data masks and display helpers
globalVariables(c(".env", ".l", ".r", ":=", "dt.l", "dt.r"))

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
  if (is.character(expr)) return(str_to_lower(expr))
  if (is_call(expr)) return(as.call(map(as.list(expr), lower_selection)))
  expr
}

# resolve ignore with tidyselect, lowering selections to match column names
resolve_ignore_cols <- \(quo, l, r) {
  cols  <- union(names(l), names(r))
  proxy <- cols |>
    set_names(cols) |>
    map(\(col) vec_ptype(l[[col]] %||% r[[col]]))
  quo   <- quo_set_expr(quo, lower_selection(quo_get_expr(quo)))
  try_fetch(
    tidyselect::eval_select(quo, proxy, allow_rename = FALSE) |> names(),
    error = \(cnd) abort("ignore must be a valid tidyselect selection", parent = cnd)
  )
}

# check a frame, lower-case its names, reject duplicates the lowercasing creates
validate_frame <- \(d, name) {
  if (!is.data.frame(d)) {
    abort(glue("{name} must be a data frame, not {class(d)[1]}"))
  }
  if (nrow(d) == 0) {
    abort(glue("{name} must not be empty"))
  }
  if (ncol(d) == 0) {
    abort(glue("{name} must have at least one column"))
  }
  d     <- d |>
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
    warn(glue("about {f_num(cells)} cells to compare, may be slow"))
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
#' @param digits Decimal places for numeric tolerance, between -22 and 22.
#'   Default `4`.
#' @param strings `"strict"` (default) or a `_`-joined combination of
#'   `"ignorecase"` and `"trim"`, in any order (e.g. `"ignorecase_trim"`).
#' @param factors `"as_strings"` (default) or `"as_factors"` (compared as
#'   factors; columns whose levels differ in set or order land in `schema`).
#' @param raise_error Abort with the printed summary on differences. Default
#'   `FALSE`.
#' @param ignore Columns skipped in value comparisons, selected with
#'   [tidyselect::language] syntax: character vectors, bare names, or helpers
#'   (e.g. `c(price)`, `starts_with("cost")`). Matching ignores case. Columns
#'   must exist in at least one frame and must not be `by` columns. `NULL`
#'   (default) ignores nothing.
#' @param fn Applied to both frames before comparing; its output is ungrouped.
#'   Default `\(d) d`.
#' @param ... Not used; must be empty.
#' @param keys,round,roundDigits Aliases for `by` and `digits`; supply at
#'   most one of each pair.
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
#' message. `by` columns must be unique on each side. `NA` values in `by`
#' columns match one another, so rows with missing keys are paired; `NaN`
#' values compare equal to each other and `Inf` compares normally. `print()`
#' also reports matched rows and compared columns.
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
#' @importFrom rlang abort warn inform expr sym arg_match inherits_any is_scalar_integerish is_string is_bool enquo quo_get_expr quo_set_expr is_symbol is_call as_string try_fetch check_dots_empty check_exclusive env_get_list "%||%"
#' @importFrom dplyr rename_with select all_of mutate across row_number full_join inner_join bind_rows bind_cols filter distinct pull rename join_by cur_column ungroup where
#' @importFrom tidyr drop_na
#' @importFrom purrr map map_chr map_lgl map2 set_names discard imap
#' @importFrom stringr str_to_lower str_c str_trim str_split_1 str_detect
#' @importFrom glue glue
#' @importFrom tibble tibble
#' @importFrom stats na.omit
#' @importFrom utils globalVariables
#' @importFrom lubridate as_date hour minute second
#' @importFrom vctrs vec_ptype vec_ptype_abbr vec_ptype_common vec_equal vec_in
#'
#' @export
kompare <- \(
  l, r, by = NULL, digits = 4, strings = "strict", factors = "as_strings",
  raise_error = FALSE, ignore = NULL, fn = \(d) d,
  ...,
  keys = NULL, round = NULL, roundDigits = NULL
) {
  # dots and alias validation
  check_dots_empty()
  check_exclusive(by, keys, .require = FALSE)
  check_exclusive(round, roundDigits, .require = FALSE)
  if ((!is.null(round) || !is.null(roundDigits)) && !missing(digits)) {
    abort("specify only one of digits and its aliases round or roundDigits")
  }
  if (!is.null(keys)) by <- keys
  if (!is.null(roundDigits)) digits <- roundDigits
  if (!is.null(round)) digits <- round

  # further arg validation and parsing
  if (!is_scalar_integerish(digits) ||
    !is.finite(digits) ||
    abs(digits) > 22) {
    abort("digits must be a single integer between -22 and 22")
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

  # construct and validate by
  check_by_repeats <- \(x, side = NULL) {
    if (anyDuplicated(x) > 0) {
      repeats <- x[duplicated(x)] |>
        unique() |>
        str_c(collapse = ", ")
      where   <- if (is.null(side)) "" else glue(" on the {side} side")
      abort(glue("by repeats a column{where}: {repeats}"))
    }
  }

  check_by_exists <- \(x, cols, side) {
    missing <- setdiff(x, cols)
    if (length(missing) > 0) {
      abort(glue("by columns missing from {side}: {str_c(missing, collapse = ', ')}"))
    }
  }

  check_by_types <- \(l, r, by, origin = "by columns must have compatible types in l and r") {
    non_atomic <- discard(by, \(col) is.atomic(l[[col]]) && is.atomic(r[[col]]))
    if (length(non_atomic) > 0) {
      abort(glue(
        "by columns must be atomic, not list or other recursive types: ",
        "{str_c(non_atomic, collapse = ', ')}"
      ))
    }
    ok <- map_lgl(by, \(col) {
      try_fetch(
        {
          vec_ptype_common(l[[col]], r[[col]])
          TRUE
        },
        error = \(cnd) FALSE
      )
    })
    if (!all(ok)) {
      abort(glue("{origin}: {str_c(by[!ok], collapse = ', ')}"))
    }
  }

  if (!is.null(by) &&
    !is.character(by) &&
    !inherits_any(by, "dplyr_join_by")) {
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
    if (anyNA(lhs) || anyNA(rhs) || any(c(lhs, rhs) == "")) {
      abort("by columns must not be NA or empty strings")
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
    by_l     <- by$x |> str_to_lower()
    by_r     <- by$y |> str_to_lower()
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

    r  <- r |> rename(!!!set_names(by_r, by_l))
    by <- by_l
  }

  # apply user-specified optional function
  if (!is.function(fn)) {
    abort("fn must be a function")
  }

  apply_fn <- \(d, side) {
    out <- validate_frame(fn(d), glue("fn output for {side}"))
    if (!is.null(by)) {
      check_by_exists(by, names(out), side)
    }
    out
  }

  l <- apply_fn(l, "l")
  r <- apply_fn(r, "r")

  # midnight datetimes become dates after fn; all-na columns are not midnight
  all_midnight <- \(x) {
    !all(is.na(x)) && all(hour(x) == 0 & minute(x) == 0 & second(x) == 0, na.rm = TRUE)
  }

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
  l <- l |> unify_dates()
  r <- r |> unify_dates()

  # factor cols shared by both sides, with levels unioned for comparison;
  # rebuild after fn, which may change levels
  factor_cols <- intersect(
    l |> select(where(is.factor)) |> names(),
    r |> select(where(is.factor)) |> names()
  )
  factor_lvls <- factor_cols |>
    set_names(factor_cols) |>
    map(\(col) union(levels(l[[col]]), levels(r[[col]])))

  if (!is.null(by)) {
    # fn can change key types; validate afterwards, naming fn as the cause
    check_by_types(l, r, by, "fn left by columns with incompatible types in l and r")
  }

  # resolve columns ignored in case insensitive tidyselect
  ignore <- resolve_ignore_cols(enquo(ignore), l, r)

  # assign row number if necessary; check for key duplicates
  has_key_dupes <- \(d) any(d |> select(all_of(by)) |> duplicated())

  format_dupes <- \(d) {
    k <- d |> select(all_of(by))
    k <- k[duplicated(k), , drop = FALSE] |> head(3)
    map_chr(seq_len(nrow(k)), \(i) {
      str_c(unlist(k[i,], use.names = FALSE), collapse = "/")
    }) |>
      str_c(collapse = ", ")
  }

  n_cols <- c(ncol(l), ncol(r))
  if (is.null(by)) {
    l  <- l |> mutate(.rn = row_number())
    r  <- r |> mutate(.rn = row_number())
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
    ignored_keys <- intersect(ignore, by)
    if (length(ignored_keys) > 0) {
      abort(glue("ignore must not include by columns: {str_c(ignored_keys, collapse = ', ')}"))
    }
  }

  # find mismatching data types and factors
  lvl_mismatch <- if (factors == "as_factors") {
    l |>
      select(where(is.factor)) |>
      names() |>
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
  compared_cols <- dt |>
    filter(dt.l == dt.r) |>
    pull(col) |>
    setdiff(ignore)
  check_cells(l, r, compared_cols)

  # construct diffs by type
  neq <- \(x, y) {
    if (is.numeric(x) && is.numeric(y)) {
      x <- if (any(is.nan(x))) replace(x, is.nan(x), NA) else x
      y <- if (any(is.nan(y))) replace(y, is.nan(y), NA) else y
    }
    !vec_equal(x, y, na_equal = TRUE)
  }

  d_types <- na.omit(dt |>
                       filter(dt.l == dt.r) |>
                       distinct(dt.l) |>
                       drop_na() |>
                       pull())
  diffs   <- d_types |>
    map(\(type) {
      cols <- na.omit(
        dt |>
          filter(dt.l == type & dt.r == type) |>
          pull(col) |>
          setdiff(by) |>
          setdiff(ignore)
      )
      if (length(cols) == 0) return(tibble())

      # join once for this type, comparing every column pairwise
      comp <- inner_join(
        l |> select(all_of(c(by, cols))) |> clean_types(),
        r |> select(all_of(c(by, cols))) |> clean_types(),
        by = by, suffix = c("\x1f.l", "\x1f.r")
      )

      # compare whole columns at once, one logical mask per column
      clean_str <- \(s) {
        if (sf_ignorecase) s <- str_to_lower(s)
        if (sf_trim) s <- str_trim(s)
        s
      }

      differs <- \(x, y) {
        if (type %in% c("dbl", "int")) return(neq(round(x, digits), round(y, digits)))
        if (type == "chr") return(neq(clean_str(x), clean_str(y)))
        neq(x, y)
      }

      mask <- cols |>
        set_names(cols) |>
        map(\(col) differs(comp[[str_c(col, "\x1f.l")]], comp[[str_c(col, "\x1f.r")]]))

      # locate differing cells from the masks, then pull only those cells;
      # no pivot, so cost scales with differences rather than cells
      mask |>
        imap(\(m, col) {
          rows <- if (is.matrix(m)) which(rowSums(m) > 0) else which(m)
          if (length(rows) == 0) return(NULL)
          nm_l <- str_c(col, "\x1f.l")
          nm_r <- str_c(col, "\x1f.r")
          take <- \(x) if (is.matrix(x)) x[rows, , drop = FALSE] else x[rows]
          bind_cols(
            comp[rows, by, drop = FALSE],
            tibble(
              col = col,
              .l  = take(comp[[nm_l]]),
              .r  = take(comp[[nm_r]])
            )
          )
        }) |>
        bind_rows()
    }) |>
    set_names(d_types) |>
    discard(\(d) nrow(d) == 0)

  # rows present on only one side
  l_keys    <- l |> select(all_of(by))
  r_keys    <- r |> select(all_of(by))
  unmatched <- bind_rows(
    l_keys[!vec_in(l_keys, r_keys),] |> mutate(.l = TRUE, .r = NA),
    r_keys[!vec_in(r_keys, l_keys),] |> mutate(.l = NA, .r = TRUE)
  )

  # make summary
  summ <- structure(
    list(
      "schema" = dt |> filter(is.na(dt.l) | is.na(dt.r) | dt.l != dt.r),
      "keys"   = unmatched,
      "diffs"  = diffs
    ),
    dims     = c(nrow(l), nrow(r)),
    compared = c(
      rows = (nrow(l) + nrow(r) - nrow(unmatched)) / 2,
      cols = length(compared_cols) + length(setdiff(by, ignore)) - (".rn" %in% by)
    ),
    settings = env_get_list(
      environment(),
      c("by", "digits", "strings", "factors", "ignore", "n_cols")
    ),
    class    = "kompare"
  )

  if (!is_equal(summ) && raise_error) {
    abort(
      "kompare found differences between l and r",
      class = "kompare_error",
      obj   = summ,
      body  = .format_kompare(summ) |> str_c(collapse = "\n"),
      call  = NULL
    )
  }
  return(summ)
}
