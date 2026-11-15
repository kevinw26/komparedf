# atomic on both sides with a common type
is_validly_atomic <- \(l, r, cols) {
  cols |>
    map_lgl(\(c) {
      is.atomic(l[[c]]) &&
        is.atomic(r[[c]]) &&
        try_fetch(
          {
            vec_ptype_common(l[[c]], r[[c]])
            TRUE
          },
          error = \(cnd) FALSE
        )
    })
}

#' Suggest keys for kompare
#'
#' Suggests shared columns that uniquely identify rows in both `l` and `r`.
#'
#' @param l,r Data frames.
#' @param by Character vector of shared column names; extended, not replaced.
#'   `join_by()` objects and key renaming are rejected.
#' @param max_add Maximum number of columns to add to `by`.
#' @param max_candidates Maximum number of candidate columns to consider.
#'
#' @return An unnamed character vector naming one key: a unique single column
#'   if available, otherwise a composite extending `by`, not necessarily the
#'   shortest.
#'
#' @details
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
  check_frame(l, "l")
  check_frame(r, "r")
  if (inherits(by, "dplyr_join_by")) {
    abort("by must be a character vector of shared column names, not join_by()")
  }
  if (!is.character(by)) {
    abort("by must be a character vector of shared column names")
  }
  if (!is.null(names(by)) && any(names(by) != unname(by))) {
    abort("key renaming is not supported")
  }
  by <- unname(by)
  check_by_values(by)
  check_by_exists(by, names(l), "l")
  check_by_exists(by, names(r), "r")

  candidates <- intersect(names(l), names(r)) |>
    setdiff(by) |>
    as_tibble_col(column_name = "col") |>
    filter(is_validly_atomic(l, r, col)) |>
    mutate(
      n_l = map_int(col, \(col) vec_unique_count(l[[col]])),
      n_r = map_int(col, \(col) vec_unique_count(r[[col]]))
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
          vec_unique_count(l |> select(all_of(cols))),
          vec_unique_count(r |> select(all_of(cols)))
        )
      })) |>
      slice_max(score, n = 1, with_ties = FALSE) |>
      pull(col)
    c(k_vector, next_col)
  }

  key <- reduce(
    seq_len(max_add),
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
