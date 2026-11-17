suppressPackageStartupMessages(library(dplyr))

test_that("factors with matching levels are compared by value", {
  l <- tibble(id = 1:3, x = factor(c("a", "b", "c")))
  r <- l
  r$x[3] <- "a"

  out <- kompare(l, r, factors = "as_factors")

  expect_equal(nrow(out$diffs$fct), 1)
  expect_equal(nrow(out$schema), 0)
})

test_that("extra level on one side is a schema mismatch under as_factors", {
  l <- tibble(id = 1:3, x = factor(c("a", "b", "c")))
  r <- l |> mutate(x = factor(x, levels = c("a", "b", "c", "unknown")))
  r$x[3] <- "unknown"

  strings_out <- kompare(l, r, factors = "as_strings")
  factors_out <- kompare(l, r, factors = "as_factors")

  expect_equal(nrow(strings_out$diffs$fct), 1)
  expect_equal(factors_out$schema$dt.l, "fct[a,b,c]")
  expect_equal(factors_out$schema$dt.r, "fct[a,b,c,unknown]")
  expect_length(factors_out$diffs, 0)
})

test_that("factors = 'as_strings' compares levels as text", {
  l <- tibble(x = factor(c("a", "b"), levels = c("a", "b")))
  r <- tibble(x = factor(c("a", "B"), levels = c("a", "B")))

  # "b" vs "B" differ in strict string comparison
  expect_length(kompare(l, r, factors = "as_strings")$diffs, 1)
})

test_that("string options apply to factors compared as strings", {
  l <- tibble(id = 1:2, x = factor(c("A ", "b")))
  r <- tibble(id = 1:2, x = factor(c("a", "b")))

  expect_length(kompare(l, r, by = "id", strings = "ignorecase_trim")$diffs, 0)
  expect_equal(
    nrow(kompare(l, r, by = "id", strings = "strict")$diffs$fct),
    1
  )
  levels <- c("A ", "a", "b")
  lf <- l |> mutate(x = factor(as.character(x), levels = levels))
  rf <- r |> mutate(x = factor(as.character(x), levels = levels))

  expect_equal(
    nrow(kompare(
      lf, rf,
      by = "id", factors = "as_factors", strings = "ignorecase_trim"
    )$diffs$fct),
    1
  )
})

test_that("string options apply to ordered factors compared as strings", {
  l <- tibble(id = 1:2, x = factor(c("A ", "b"), levels = c("A ", "b"), ordered = TRUE))
  r <- tibble(id = 1:2, x = factor(c("a", "b"), levels = c("a", "b"), ordered = TRUE))

  expect_length(kompare(l, r, by = "id", strings = "ignorecase_trim")$diffs, 0)
  expect_equal(
    nrow(kompare(l, r, by = "id", strings = "strict")$diffs$ord),
    1
  )
})

test_that("as_strings ignores level definitions", {
  l <- tibble(x = factor(c("a", "b"), levels = c("a", "b")))
  r <- tibble(x = factor(c("a", "b"), levels = c("b", "a")))

  expect_length(kompare(l, r, factors = "as_strings")$diffs, 0)
})

test_that("factor level order matters under as_factors", {
  l <- tibble(x = factor(c("a", "b"), levels = c("a", "b")))
  r <- tibble(x = factor(c("a", "b"), levels = c("b", "a")))

  out <- kompare(l, r, factors = "as_factors")

  expect_equal(nrow(out$schema), 1)
  expect_equal(out$schema$col, "x")
  expect_equal(out$schema$dt.l, "fct[a,b]")
  expect_equal(out$schema$dt.r, "fct[b,a]")
  expect_length(out$diffs, 0)
})

test_that("as_strings ignores ordered factor level definitions", {
  l <- tibble(x = factor(c("a", "b"), levels = c("a", "b"), ordered = TRUE))
  r <- tibble(x = factor(c("a", "b"), levels = c("b", "a"), ordered = TRUE))

  expect_length(kompare(l, r, factors = "as_strings")$diffs, 0)
})

test_that("ordered factor level order matters under as_factors", {
  l <- tibble(x = factor(c("a", "b"), levels = c("a", "b"), ordered = TRUE))
  r <- tibble(x = factor(c("a", "b"), levels = c("b", "a"), ordered = TRUE))

  out <- kompare(l, r, factors = "as_factors")

  expect_equal(nrow(out$schema), 1)
  expect_equal(out$schema$dt.l, "ord[a,b]")
  expect_equal(out$schema$dt.r, "ord[b,a]")
  expect_length(out$diffs, 0)
})

test_that("opposite ordered factor keys are incompatible", {
  l <- tibble(f = factor(c("a", "b"), levels = c("a", "b"), ordered = TRUE))
  r <- tibble(f = factor(c("a", "b"), levels = c("b", "a"), ordered = TRUE))

  cnd <- tryCatch(kompare(l, r, by = "f"), error = identity)

  expect_match(
    conditionMessage(cnd),
    "f (ord[a,b] vs ord[b,a])",
    fixed = TRUE
  )
})

test_that("factor in one frame only lands in schema", {
  l <- tibble(id = 1:2, x = factor(c("a", "b")))
  r <- tibble(id = 1:2, x = c("a", "b"))

  out <- kompare(l, r)

  expect_equal(nrow(out$schema), 1)
  expect_equal(out$schema$col, "x")
})

test_that("as_factors uses levels after fn, not before", {
  l <- tibble(id = 1:2, f = factor(c("a", "a")))
  r <- tibble(id = 1:2, f = factor(c("a", "b")))
  fn <- \(d) mutate(d, f = factor(if_else(f == "a", "x", "y"), levels = c("x", "y")))

  out <- kompare(l, r, by = "id", factors = "as_factors", fn = fn)

  expect_equal(nrow(out$diffs$fct), 1)
  expect_equal(out$diffs$fct$id, 2)
})
