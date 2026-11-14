suppressPackageStartupMessages(library(dplyr))

test_that("digits sets the numeric tolerance", {
  l <- tibble::tibble(id = 1:2, x = c(1.4, 2.4))
  r <- l |> mutate(x = x + 1e-6)
  wide <- l |> mutate(x = x + 0.4)

  expect_length(kompare(l, r, by = "id")$diffs, 0)
  expect_length(kompare(l, r, by = "id", digits = 8)$diffs, 1)
  expect_length(kompare(l, wide, by = "id", digits = 0)$diffs, 1)
})

test_that("strings = 'strict' compares exactly", {
  l <- tibble::tibble(id = 1:2, s = c("a", "b"))
  r <- l |> mutate(s = c("A ", "b"))

  expect_equal(nrow(kompare(l, r, by = "id")$diffs$chr), 1)
  expect_length(kompare(l, r, by = "id", strings = "loose")$diffs, 0)
})

test_that("ignore drops columns from value comparison", {
  l <- tibble::tibble(id = 1:2, x = c(1, 2), updated = c(1, 2))
  r <- l |> mutate(updated = updated + 1)

  expect_equal(nrow(kompare(l, r, by = "id")$diffs$dbl), 2)
  expect_length(kompare(l, r, by = "id", ignore = "updated")$diffs, 0)

  retyped <- l |> mutate(updated = as.character(updated))
  expect_equal(nrow(kompare(l, retyped, by = "id", ignore = "updated")$schema), 1)
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
