suppressPackageStartupMessages(library(dplyr))

test_that("strings = 'strict' compares exactly", {
  l <- tibble::tibble(id = 1:2, s = c("a", "b"))
  r <- l |> mutate(s = c("A ", "b"))

  expect_equal(nrow(kompare(l, r, by = "id")$diffs$chr), 1)
  # each flag alone still differs
  expect_equal(nrow(kompare(l, r, by = "id", strings = "ignorecase")$
                      diffs$
                      chr), 1)
  expect_equal(nrow(kompare(l, r, by = "id", strings = "trim")$
                      diffs$
                      chr), 1)
  # combined, in any order, matches
  expect_length(kompare(l, r, by = "id", strings = "ignorecase_trim")$diffs, 0)
  expect_length(kompare(l, r, by = "id", strings = "trim_ignorecase")$diffs, 0)
})

test_that("strings is stored lower-cased", {
  l <- tibble::tibble(id = 1, x = "a")

  out <- kompare(l, l, by = "id", strings = "TRIM")

  expect_equal(attr(out, "settings")$strings, "trim")
  expect_true(as.logical(kompare(l, l, by = "id", strings = "STRICT")))
})

test_that("fn is applied to both sides before comparing", {
  l <- tibble::tibble(id = 1:3, x = c(3, 1, 2))
  r <- tibble::tibble(id = 3:1, x = c(2, 1, 3))

  expect_equal(nrow(kompare(l, r)$diffs$dbl), 2)
  expect_length(kompare(l, r, fn = \(d) arrange(d, id))$diffs, 0)
})

test_that("raise_error throws and prints the summary on differences", {
  l <- tibble::tibble(id = 1:2, x = c(1, 2))
  r <- l |> mutate(x = x + 1)

  expect_error(kompare(l, r, by = "id", raise_error = TRUE), "kompare found differences")
  expect_error(kompare(l, r, by = "id", raise_error = TRUE), "schema mismatches")
  expect_no_error(kompare(l, l, by = "id", raise_error = TRUE))
})

test_that("positional comparison lines rows up by position", {
  l <- tibble::tibble(x = 1:3)
  r <- tibble::tibble(x = c(1L, 9L, 3L, 4L))

  out <- kompare(l, r)

  expect_equal(nrow(out$keys), 1)
  expect_equal(out$diffs$int$.rn, 2)
})

test_that("strings must be a single string", {
  l <- tibble::tibble(id = 1, x = "a")

  expect_error(kompare(l, l, by = "id", strings = NULL), "strings must be a single string")
  expect_error(
    kompare(l, l, by = "id", strings = c("trim", "ignorecase")),
    "strings must be a single string"
  )
})

test_that("raise_error must be a logical", {
  l <- tibble::tibble(id = 1, x = 1)

  expect_error(kompare(l, l, by = "id", raise_error = NA), "raise_error must be")
  expect_error(kompare(l, l, by = "id", raise_error = "yes"), "raise_error must be")
})

test_that("fn must be a function returning a frame with the by columns", {
  l <- tibble::tibble(id = 1, x = 1)

  expect_error(kompare(l, l, by = "id", fn = NULL), "fn must be a function")
  expect_error(kompare(l, l, by = "id", fn = \(d) d$id), "must be a data frame")
  expect_error(kompare(l, l, by = "id", fn = \(d) select(d, x)), "missing from l")
  expect_error(kompare(l, l, by = "id", fn = \(d) mutate(d, .rn = 1)), "reserved column name")
})

test_that("fn output is ungrouped", {
  l <- tibble::tibble(id = 1:2, g = c("a", "b"), x = c(1, 2))
  r <- l |> mutate(x = c(9, 2))

  out <- kompare(l, r, by = "id", fn = \(d) group_by(d, g))

  expect_equal(nrow(out$diffs$dbl), 1)
  expect_equal(out$diffs$dbl$id, 1)
})
