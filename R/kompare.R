#' Compare two data frames
#'
#' Compares two data frames for differences in schema (column names/types),
#' keys (rows present in one but not the other), and values. Column names
#' are matched case-insensitively, and compatible date/datetime columns are
#' unified before comparison.
#'
#' @param l A data frame (the "left" side of the comparison).
#' @param r A data frame (the "right" side of the comparison).
#' @param keys Character vector of column names that uniquely identify a row
#'   in both `l` and `r`, or a [dplyr::join_by()] object for asymmetric
#'   column names (eg `join_by(l_id == r_id)`), matched the same way as
#'   `left_join(l, r, join_by(...))`. If `NULL` (the default), rows are
#'   compared by position instead, which assumes both data frames are
#'   already in the same row order.
#' @param .round Number of decimal places of tolerance to use when comparing
#'   numeric/integer columns. Differences smaller than `10^-.round` are
#'   treated as equal. Default is `4`.
#' @param .strict_strings If `FALSE`, character columns are compared after
#'   lowercasing and trimming whitespace, so differences in case or leading/
#'   trailing spaces are ignored. Default is `TRUE` (exact comparison).
#' @param .raise_error If `TRUE`, print a summary and throw an error when
#'   any differences are found (schema mismatches, unmatched keys, or value
#'   differences). Default is `FALSE`.
#' @param .ignore Character vector of column names to exclude from value
#'   comparison entirely (eg columns expected to always differ, like an
#'   `updated_at` timestamp). Excluded columns can still appear in `schema`
#'   if their types mismatch.
#' @param .fn A function to apply to both data frames before comparing
#'   (eg [dplyr::arrange_all()]). Executes after column names are forced to
#'   lower case and renaming, if any, by `keys`.
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
#'     columns are compared exactly unless `.strict_strings = FALSE`; all
#'     other types are compared for exact equality. In every case, a value
#'     that is `NA` on one side and non-`NA` on the other also counts as a
#'     difference. Types with zero differences are omitted.}
#' }
#'
#' @details
#' Only columns that share the same name (case-insensitive) and the same
#' data type in both `l` and `r` are compared for value differences; columns
#' that differ in type, or exist in only one data frame, are reported in
#' `schema` instead and excluded from `diffs`. Factor columns are compared
#' as character but reported under their original type in `schema`.
#' `POSIXct`/`POSIXlt` columns with no time-of-day component (i.e. every
#' non-`NA` value falls at midnight) are converted to `Date` before
#' comparison, with a warning identifying the affected column.
#'
#' If `keys` is supplied, both `l` and `r` must have no duplicate rows with
#' respect to those key columns, or `kompare()` will error.
#'
#' @examples
#' l <- data.frame(id = 1:3, x = c(1, 2, 3), y = c("a", "b", "c"))
#' r <- data.frame(id = 1:3, x = c(1, 2.5, 3), y = c("a", "B", "z"))
#'
#' result <- kompare(l, r, keys = "id")
#' result$schema
#' result$keys
#' result$diffs
#'
#' # without keys, rows are compared by position
#' kompare(l, r)
#'
#' # ignore whitespace/case differences in strings
#' kompare(l, r, keys = "id", .strict_strings = FALSE)
#'
#' # error immediately if any differences are found
#' \dontrun{
#' kompare(l, r, keys = "id", .raise_error = TRUE)
#' }
#'
#' @importFrom dplyr rename_with select all_of mutate row_number full_join filter distinct pull rename cur_column
#' @importFrom tidyr pivot_longer drop_na
#' @importFrom purrr map map_chr set_names discard
#' @importFrom stringr str_to_lower str_c str_trim
#' @importFrom glue glue
#' @importFrom tibble tibble
#' @importFrom stats na.omit
#' @importFrom lubridate as_date hour minute second
#'
#' @export
kompare <- \(
  l, r, keys = NULL, .round = 4, .strict_strings = TRUE, .raise_error = FALSE,
  .ignore = c(), .fn = \(d) d
) {
  # helper functions
  has_dupes <- \(d) any(d |> select(all_of(keys)) |> duplicated())
  clean_types <- \(d) d |> mutate(across(where(is.factor), as.character))
  unify_dates <- \(d) d |> mutate(across(
    where(\(x) inherits(x, "POSIXt")),
    \(x) if (all(hour(x) == 0 & minute(x) == 0 & second(x) == 0, na.rm = TRUE)) {
      warning(glue("column {cur_column()} has no time component and was converted to lubridate date"))
      as_date(x)
    } else {
      x
    }
  ))

  # coerce names to lower case, coerce dates, apply the function
  l <- l |>
    rename_with(str_to_lower) |>
    unify_dates()
  r <- r |>
    rename_with(str_to_lower) |>
    unify_dates()

  # allow keys to be a join_by() object for asymmetric l/r column names,
  # same as left_join(l, r, join_by(a, b)); rename r's key cols to match l
  if (inherits(keys, "dplyr_join_by")) {
    keys$x <- keys$x |> str_to_lower()
    keys$y <- keys$y |> str_to_lower()
    r <- r |> rename(!!!set_names(keys$y, keys$x))
    keys <- keys$x
  } else if (!is.null(keys)) {
    keys <- keys |> str_to_lower()
  }

  # apply function
  l <- l |> .fn()
  r <- r |> .fn()

  # assign row number if necessary; check for key duplicates
  if (is.null(keys)) {
    l <- l |> mutate(.rn = row_number())
    r <- r |> mutate(.rn = row_number())
    keys <- ".rn"
  } else {
    if (has_dupes(l)) stop(glue("keys {paste(keys, collapse = ', ')} must uniquely identify all rows in l"))
    if (has_dupes(r)) stop(glue("keys {paste(keys, collapse = ', ')} must uniquely identify all rows in r"))
  }
  dt <- full_join(
    tibble(col = names(l), dt.l = map_chr(l, \(x) class(x)[1])),
    tibble(col = names(r), dt.r = map_chr(r, \(x) class(x)[1])),
    by = "col"
  ) |>
    filter(!col %in% keys)

  d_types <- na.omit(dt |> filter(dt.l == dt.r) |> distinct(dt.l) |> drop_na() |> pull())
  diffs <- d_types |>
    map(\(type) {
      cols <- na.omit(
        dt |> filter(dt.l == type & dt.r == type) |> pull(col) |>
          setdiff(keys) |> setdiff(.ignore)
      )
      comp <- full_join(
        l |> select(all_of(c(keys, cols))) |> clean_types(),
        r |> select(all_of(c(keys, cols))) |> clean_types(),
        by = keys, suffix = c("\x1f.l", "\x1f.r")
      ) |>
        pivot_longer(-all_of(keys), names_sep = "\x1f", names_to = c("col", ".value"))
      if (type %in% c("numeric", "integer")) {
        comp |>
          mutate(.l = round(.l, .round), .r = round(.r, .round)) |>
          filter(.l != .r | is.na(.l) != is.na(.r))
      } else if (type == "character" && !.strict_strings) {
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

  summ <- structure(
    list(
      "schema" = dt |> filter(is.na(dt.l) | is.na(dt.r) | dt.l != dt.r),
      "keys" = full_join(
        l |> select(all_of(keys)) |> mutate(.l = TRUE),
        r |> select(all_of(keys)) |> mutate(.r = TRUE),
        by = keys
      ) |> filter(is.na(.l) | is.na(.r)),
      "diffs" = diffs
    ),
    dims = c(nrow(l), nrow(r)),
    class = "kompare"
  )

  has_diffs <- nrow(summ$schema) > 0 || nrow(summ$keys) > 0 || length(summ$diffs) > 0
  if (has_diffs && .raise_error) {
    summ |> summary()
    stop("kompare found differences between l and r")
  }
  return(summ)
}
