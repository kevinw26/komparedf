#' Suggest keys for kompare
#'
#' Suggests shared columns that uniquely identify rows in both `l` and `r`.
#'
#' @param l,r Data frames.
#' @param by Character vector of shared column names; extended, not replaced.
#'   `join_by()` objects and key renaming are rejected.
#' @param max_add Maximum number of columns to add to `by`; a single
#'   non-negative integer.
#' @param max_candidates Maximum number of candidate columns to consider; a
#'   single non-negative integer.
#'
#' @return An unnamed character vector naming one key: a unique single column
#'   if available, otherwise a composite extending `by`, not necessarily the
#'   shortest.
#'
#' @details
#' Names are matched case-insensitively, and returned names are lower case.
#' Candidates are shared, atomic, type-compatible, non-constant columns,
#' limited to `max_candidates` with the highest weaker-side cardinality. A single column
#' is returned if possible; otherwise a key is grown greedily from `by`. The
#' search is not exhaustive.
#'
#' @export
#' @examples
#' l <- tibble::tibble(id = c(1, 1, 2, 2), grp = c("a", "b", "a", "b"))
#' suggest_kompare_keys(l, l, by = "id")
suggest_kompare_keys <- \(l, r, by = character(), max_add = 5, max_candidates = 20) {
  l <- validate_frame(l, "l")
  r <- validate_frame(r, "r")
  if (inherits(by, "dplyr_join_by")) {
    abort("by must be a character vector of shared column names, not join_by()")
  }
  if (!is.character(by)) {
    abort("by must be a character vector of shared column names")
  }
  by_names <- names(by)
  if (!is.null(by_names) &&
    (anyNA(by_names) || any(str_to_lower(by_names) != str_to_lower(unname(by))))) {
    abort("key renaming is not supported")
  }
  by <- unname(by) |> str_to_lower()
  check_by(l, r, by)
  max_add <- check_int(max_add, "max_add", min = 0, cap = TRUE)
  max_candidates <- check_int(max_candidates, "max_candidates", min = 0, cap = TRUE)

  candidates <- intersect(names(l), names(r)) |>
    setdiff(by) |>
    setdiff(c("col", ".l", ".r")) |>
    as_tibble_col(column_name = "col") |>
    filter(
      is_atomic_pair(l, r, col) &
        map_lgl(col, \(c) has_common_dtype(l[[c]], r[[c]]))
    ) |>
    mutate(
      n_l = map_int(col, \(col) l[[col]] |> vec_unique_count()),
      n_r = map_int(col, \(col) r[[col]] |> vec_unique_count())
    ) |>
    filter(n_l > 1, n_r > 1) |>
    mutate(score = pmin(n_l, n_r)) |>
    slice_max(score, n = max_candidates, with_ties = FALSE) |>
    pull(col)

  keys_uniquely_identify <- \(k_vector) {
    length(k_vector) > 0 &&
      vec_unique_count(l |> select(all_of(k_vector))) == nrow(l) &&
      vec_unique_count(r |> select(all_of(k_vector))) == nrow(r)
  }

  try_best_candidate <- \(k_vector) {
    if (length(k_vector) >= length(by) + max_add) return(k_vector)
    next_col <- tibble(col = setdiff(candidates, k_vector)) |>
      mutate(score = map_int(col, \(col) {
        cols <- c(k_vector, col)
        min(
          l |> select(all_of(cols)) |> vec_unique_count(),
          r |> select(all_of(cols)) |> vec_unique_count()
        )
      })) |>
      slice_max(score, n = 1, with_ties = FALSE) |>
      pull(col)
    c(k_vector, next_col)
  }

  key <- reduce(
    seq_len(min(max_add, length(candidates))),
    \(key, i) if (keys_uniquely_identify(key)) key else try_best_candidate(key),
    .init = by
  )

  singles <- candidates |>
    keep(keys_uniquely_identify)
  if (length(singles) > 0) return(singles[1])
  if (keys_uniquely_identify(key)) return(key)

  abort(glue(
    "no obvious unique key found in the top {max_candidates} compatible shared columns ",
    "plus up to {max_add} added to `by` greedily; add a row id or deduplicate"
  ))
}

# return NULL when the search errors or takes more than 3 seconds
try_suggest_kompare_keys <- \(l, r, by, ...) {
  setTimeLimit(elapsed = 3, transient = TRUE)
  on.exit(setTimeLimit(elapsed = Inf, transient = FALSE))
  try_fetch(suggest_kompare_keys(l, r, by, ...), error = \(cnd) NULL)
}
