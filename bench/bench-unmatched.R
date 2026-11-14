# manual benchmark: deriving unmatched keys from two 1m-row frames at 75% join
# run with: Rscript bench/bench-unmatched.R

suppressPackageStartupMessages({
  library(dplyr)
  library(tibble)
  library(vctrs)
})

n <- 1000000L
n_shared <- 750000L
n_only <- n - n_shared

shared <- seq_len(n_shared)
l_only <- n_shared + seq_len(n_only)
r_only <- n_shared + n_only + seq_len(n_only)

key <- \(ids) tibble(id = ids, grp = ids %% 5L)

l_keys <- key(c(shared, l_only))
r_keys <- key(c(shared, r_only))
by <- c("id", "grp")

stopifnot(
  nrow(l_keys) == n,
  nrow(r_keys) == n,
  nrow(inner_join(l_keys, r_keys, by = by)) == n_shared
)

unmatched_full <- \(l, r) {
  full_join(l |> mutate(.l = TRUE), r |> mutate(.r = TRUE), by = by) |>
    filter(is.na(.l) | is.na(.r))
}

unmatched_anti <- \(l, r) {
  bind_rows(
    l |> anti_join(r, by = by) |> mutate(.l = TRUE, .r = NA),
    r |> anti_join(l, by = by) |> mutate(.l = NA, .r = TRUE)
  )
}

unmatched_vec <- \(l, r) {
  in_r <- vec_in(l, r)
  in_l <- vec_in(r, l)
  bind_rows(
    l[!in_r, ] |> mutate(.l = TRUE, .r = NA),
    r[!in_l, ] |> mutate(.l = NA, .r = TRUE)
  )
}

normalise <- \(d) {
  d |>
    arrange(id, grp) |>
    select(id, grp, .l, .r) |>
    as.data.frame()
}

ref <- normalise(unmatched_full(l_keys, r_keys))
for (fn in list(unmatched_anti, unmatched_vec)) {
  stopifnot(isTRUE(all.equal(normalise(fn(l_keys, r_keys)), ref, check.attributes = FALSE)))
}
cat("all three produce the same", nrow(ref), "unmatched rows\n\n")

time_it <- \(label, fn, reps = 3L) {
  elapsed <- vapply(seq_len(reps), \(i) {
    gc()
    system.time(fn(l_keys, r_keys))[["elapsed"]]
  }, numeric(1))
  cat(sprintf(
    "%-8s min %6.2fs  median %6.2fs  max %6.2fs\n",
    label, min(elapsed), median(elapsed), max(elapsed)
  ))
}

cat("elapsed time over", 3L, "runs:\n")
time_it("full", unmatched_full)
time_it("anti", unmatched_anti)
time_it("vec_in", unmatched_vec)
