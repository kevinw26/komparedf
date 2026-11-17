# names used in data masks and display helpers
globalVariables(c(".env", ".l", ".r", ":=", "dt.l", "dt.r", "n_l", "n_r", "score"))

# check a frame, lower-case its names, reject duplicates the lowercasing creates
validate_frame <- \(d, name) {
  check_frame(d, name)
  if (inherits(d, "data.table")) {
    warn(glue("{name} is a data table; memory-intensive conversion to tibble"))
  }
  d <- d |>
    as_tibble(.name_repair = "minimal") |>
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
#'   Default `4`. `NULL` compares exactly, without tolerance. If digits
#'   requested require precision greater than available, a warning is emitted.
#'   Integer and integer64 columns, including matrix and array columns, are
#'   rounded to `digits` places before comparing.
#' @param strings `"strict"` (default) or a `_`-joined combination of
#'   `"ignorecase"` and `"trim"`, in any order (e.g. `"ignorecase_trim"`).
#'   Also applies when factors are compared as strings.
#' @param factors `"as_strings"` (default) or `"as_factors"` (compared as
#'   factors; columns whose levels differ in set or order land in `schema`).
#' @param raise_error Abort with the printed summary on differences. Default
#'   `FALSE`.
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
#' Names are matched case-insensitively via lowercasing; frames with columns
#' that clash after lowercasing trigger an error. Only columns sharing name
#' and type are compared for values. With `factors = "as_factors"`, factor
#' levels are treated as metadata and differing levels land in `schema`.
#' Identically named `Date` and `POSIXct` columns at midnight on both sides
#' become `Date` to avoid false differences between date-like and time-like
#' storage. `by` columns must be unique on each side. `NA` values in `by`
#' columns match one another so that rows with missing keys are paired rather
#' than reported as unmatched. `NaN` values compare equal and `Inf` compares
#' normally. The optional function `fn` is applied before values are compared; it
#' must keep any columns named in `by` and leave their types compatible, though
#' it may change them to a common type. Numeric differences are tolerated up to
#' half a unit in the `digits` decimal place, compared against the stored gap.
#' `dbl` columns warn when their resolution is coarser than that tolerance;
#' integer and integer64 columns are rounded to `digits` places before
#' comparing. The same rounding and tolerance apply to integer and double
#' matrix and array columns. `print()` also reports matched rows and compared
#' columns.
#'
#' @section Error conditions:
#' `raise_error = TRUE` gives `kompare_error` with `body` and `obj`. `kompare_dupes_error` 
#' may suggest unique comparison key if quickly found.
#'
#' @examples
#' l <- tibble::tibble(id = 1:3, x = c(1, 2, 3))
#' r <- tibble::tibble(id = 1:3, x = c(1, 2.5, 3))
#' kompare(l, r, dplyr::join_by(id))
#'
#' @importFrom rlang abort warn inform expr sym arg_match inherits_any is_scalar_integerish is_string is_bool try_fetch check_dots_empty check_exclusive env_get_list
#' @importFrom dplyr bind_cols bind_rows case_when cur_column distinct filter full_join if_any if_else inner_join join_by mutate pull rename rename_with row_number select slice_max ungroup where across all_of
#' @importFrom tidyr drop_na
#' @importFrom purrr discard imap keep map map2 map2_lgl map_chr map_int map_lgl reduce set_names
#' @importFrom stringr str_to_lower str_c str_trim str_split_1 str_detect
#' @importFrom glue glue
#' @importFrom tibble as_tibble as_tibble_col tibble
#' @importFrom stats na.omit
#' @importFrom utils globalVariables
#' @importFrom lubridate as_date hour minute second
#' @importFrom vctrs vec_equal vec_in vec_ptype vec_ptype_abbr vec_ptype_common vec_slice vec_unique_count
#'
#' @export
kompare <- \(
  l, r, by = NULL, digits = 4, strings = "strict", factors = "as_strings",
  raise_error = FALSE, fn = \(d) d,
  ...,
  keys = NULL, round = NULL, roundDigits = NULL
) {
  # dots and alias validation
  check_dots_empty()
  check_exclusive(by, keys, .require = FALSE)
  check_exclusive(digits, round, roundDigits, .require = FALSE)
  if (!missing(keys)) by <- keys
  if (!missing(roundDigits)) digits <- roundDigits
  if (!missing(round)) digits <- round

  # further arg validation and parsing
  digits <- check_int(digits, "digits", min = -22, max = 22, allow_null = TRUE)
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
  if ((setdiff(sf, c("ignorecase", "trim")) |> length()) > 0) {
    abort(glue('strings must be "strict" or a combination of ignorecase and trim, not "{strings}"'))
  }
  sf_ignorecase <- "ignorecase" %in% sf
  sf_trim <- "trim" %in% sf

  # validate data frame inputs
  l <- validate_frame(l, "l")
  r <- validate_frame(r, "r")

  # helper functions
  clean_types <- \(d) {
    if (factors == "as_strings") {
      return(d |> mutate(across(where(is.factor), as.character)))
    }
    d |> mutate(
      across(
        all_of(intersect(factor_cols, names(d))),
        \(x) factor(x, levels = factor_lvls[[cur_column()]])
      ))
  }

  # construct and validate by
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
    check_by_values(c(lhs, rhs))
    by <- join_by(!!!map2(lhs, rhs, \(x, y) expr(!!sym(x) == !!sym(y))))
  }

  # allow by to be a join_by() object for asymmetric l/r column names,
  # same as left_join(l, r, by); rename r's cols to match l
  if (inherits_any(by, "dplyr_join_by")) {
    if (any(by$condition != "==") || any(by$filter != "none")) {
      conditions <- str_c(by$condition |> unique(), collapse = ", ")
      abort(glue("by must contain only equality conditions (==), not: {conditions}"))
    }
    by_l <- by$x |> str_to_lower()
    by_r <- by$y |> str_to_lower()
    check_by(l, r, by_l, by_r, check_types = missing(fn)) # check types if no fn provided

    clash <- setdiff(by_l, by_r) |> intersect(names(r))
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

  if (!missing(fn)) {
    apply_fn <- \(d, side) fn(d) |> validate_frame(glue("fn output for {side}"))
    l <- apply_fn(l, "l")
    r <- apply_fn(r, "r")
  }

  # identically named columns holding only midnight datetimes become dates
  date_cols <- \(d) d |>
    select(where(\(x) {
      inherits_any(x, c("Date", "POSIXt")) &&
        !all(is.na(x)) &&
        all(hour(x) == 0 & minute(x) == 0 & second(x) == 0, na.rm = TRUE)
    })) |>
    names()

  cols_to_date <- date_cols(l) |> intersect(date_cols(r))
  if (length(cols_to_date) > 0) {
    inform(glue(
      "columns {str_c(cols_to_date, collapse = ', ')} have no time component and were converted ",
      "to lubridate date"))
  }
  l <- l |> mutate(across(all_of(cols_to_date), as_date))
  r <- r |> mutate(across(all_of(cols_to_date), as_date))

  # factor cols shared by both sides, with levels unioned for comparison;
  # rebuild after fn, which may change levels
  factor_cols <- intersect(
    l |> select(where(is.factor)) |> names(),
    r |> select(where(is.factor)) |> names()
  )
  factor_lvls <- factor_cols |>
    set_names(factor_cols) |>
    map(\(col) union(l[[col]] |> levels(), r[[col]] |> levels()))

  # after function execution, date inference and factor rebuild, check the keys again
  if (!missing(fn)) {
    check_by(
      l, r, by,
      type_origin = "fn left by columns with incompatible types in l and r"
    )
  }

  # assign row number if necessary; check for key duplicates
  has_key_dupes <- \(d) any(d |> select(all_of(by)) |> duplicated())

  abort_dupes <- \(d, side) {
    found <- try_suggest_kompare_keys(l, r, by, max_add = 3, max_candidates = 10)
    message <- glue(
      "by {str_c(by, collapse = ', ')} is not distinct in {side}; ",
      "row-by-row comparison requires distinct keys"
    )
    if (!is.null(found)) {
      cols <- found |>
        map_chr(\(col) sym(col) |> deparse(width.cutoff = 500L)) |>
        str_c(collapse = ", ")
      message <- glue("{message}; consider join_by({cols})")
    }
    abort(message, class = "kompare_dupes_error")
  }

  n_cols <- c(ncol(l), ncol(r))
  if (is.null(by)) {
    l <- l |> mutate(.rn = row_number())
    r <- r |> mutate(.rn = row_number())
    by <- ".rn"
  } else {
    if (has_key_dupes(l)) abort_dupes(l, "l")
    if (has_key_dupes(r)) abort_dupes(r, "r")
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
    vec_ptype_abbr(d[[col]]) |> str_c(tag)
  }

  dt <- full_join(
    tibble(col = names(l), dt.l = map_chr(names(l), \(col) col_type(l, col))),
    tibble(col = names(r), dt.r = map_chr(names(r), \(col) col_type(r, col))),
    by = "col"
  ) |>
    filter(!col %in% by)

  # abbreviated labels can agree while the underlying dtypes are incompatible
  col_has_common_dtype <- \(col) {
    x <- l[[col]]
    y <- r[[col]]
    if (factors == "as_strings") {
      if (is.factor(x)) x <- as.character(x)
      if (is.factor(y)) y <- as.character(y)
    }
    has_common_dtype(x, y)
  }

  compared_cols <- dt |>
    filter(dt.l == dt.r) |>
    pull(col) |>
    keep(col_has_common_dtype)

  # warn if data is very large
  check_cells(l, r, compared_cols)

  # construct diffs by type
  d_types <- dt |>
    filter(col %in% compared_cols) |>
    distinct(dt.l) |>
    pull()
  diffs <- d_types |>
    map(\(type) {
      cols <- dt |>
        filter(
          col %in% compared_cols &
            dt.l == type &
            dt.r == type) |>
        pull(col) |>
        setdiff(by) |>
        na.omit()
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

      differs <- \(x, y, col) {
        if (type == "chr" || (factors == "as_strings" && type %in% c("fct", "ord"))) {
          raw <- neq(clean_str(x), clean_str(y))
        } else if (is_roundable_numeric(x)) {
          raw <- numeric_differs(x, y, digits, col)
        } else {
          raw <- neq(x, y)
        }

        # test for being scalar; if matrix, do rowSums
        if (is.null(dim(raw))) raw else rowSums(raw, dims = 1) > 0
      }

      mask <- cols |>
        set_names(cols) |>
        map(\(col) differs(comp[[str_c(col, "\x1f.l")]], comp[[str_c(col, "\x1f.r")]], col))

      # locate differing cells from the masks, then pull only those cells;
      # no pivot, so cost scales with differences rather than cells
      mask |>
        imap(\(m, col) {
          rows <- which(m)
          if (length(rows) == 0) return(NULL)
          nm_l <- str_c(col, "\x1f.l")
          nm_r <- str_c(col, "\x1f.r")
          take <- \(x) vec_slice(x, rows)

          bind_cols(
            comp[rows, by, drop = FALSE],
            tibble(
              col = col,
              .l = comp[[nm_l]] |> take(),
              .r = comp[[nm_r]] |> take()
            )
          )
        }) |>
        bind_rows()
    }) |>
    set_names(d_types) |>
    discard(\(d) nrow(d) == 0)

  # rows present on only one side
  l_keys <- l |> select(all_of(by))
  r_keys <- r |> select(all_of(by))
  unmatched <- bind_rows(
    l_keys[!vec_in(l_keys, r_keys),] |> mutate(.l = TRUE, .r = NA),
    r_keys[!vec_in(r_keys, l_keys),] |> mutate(.l = NA, .r = TRUE)
  )

  # make summary
  summ <- structure(
    list(
      "schema" = dt |> filter(
        if_any(c(dt.l, dt.r), is.na) |
          !col %in% compared_cols |
          dt.l != dt.r),
      "keys" = unmatched,
      "diffs" = diffs
    ),
    dims = c(nrow(l), nrow(r)),
    compared = c(
      rows = (nrow(l) + nrow(r) - nrow(unmatched)) / 2,
      cols = length(compared_cols) + length(by) - (".rn" %in% by)
    ),
    settings = env_get_list(
      environment(),
      c("by", "digits", "strings", "factors", "n_cols")
    ),
    class = "kompare"
  )

  if (!as.logical(summ) && raise_error) {
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
