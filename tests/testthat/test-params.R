suppressPackageStartupMessages(library(dplyr))

test_that("digits sets the numeric tolerance", {
  l <- tibble::tibble(id = 1:2, x = c(1.4, 2.4))
  r <- l |> mutate(x = x + 1e-6)
  wide <- l |> mutate(x = x + 0.4)

  expect_length(kompare(l, r, by = "id")$diffs, 0)
  expect_length(kompare(l, r, by = "id", digits = 8)$diffs, 1)
  expect_length(kompare(l, wide, by = "id", digits = 0)$diffs, 1)
  expect_length(kompare(l, wide, by = "id", digits = -1)$diffs, 0)
})

test_that("digits is bounded to the range where rounding is meaningful", {
  l <- tibble::tibble(id = 1, x = 1)
  r <- tibble::tibble(id = 1, x = 9)

  # would coerce to NA and silently match every numeric column
  expect_error(kompare(l, r, by = "id", digits = 1e10), "between -22 and 22")
  expect_error(kompare(l, r, by = "id", digits = 23), "between -22 and 22")
  expect_error(kompare(l, r, by = "id", digits = -23), "between -22 and 22")
})

test_that("strings = 'strict' compares exactly", {
  l <- tibble::tibble(id = 1:2, s = c("a", "b"))
  r <- l |> mutate(s = c("A ", "b"))

  expect_equal(nrow(kompare(l, r, by = "id")$diffs$chr), 1)
  # each flag alone still differs
  expect_equal(nrow(kompare(l, r, by = "id", strings = "ignorecase")$diffs$chr), 1)
  expect_equal(nrow(kompare(l, r, by = "id", strings = "trim")$diffs$chr), 1)
  # combined, in any order, matches
  expect_length(kompare(l, r, by = "id", strings = "ignorecase_trim")$diffs, 0)
  expect_length(kompare(l, r, by = "id", strings = "trim_ignorecase")$diffs, 0)
})

test_that("ignore drops columns from value comparison", {
  l <- tibble::tibble(id = 1:2, x = c(1, 2), updated = c(1, 2))
  r <- l |> mutate(updated = updated + 1)

  expect_equal(nrow(kompare(l, r, by = "id")$diffs$dbl), 2)
  expect_length(kompare(l, r, by = "id", ignore = "updated")$diffs, 0)
  expect_equal(attr(kompare(l, r, by = "id", ignore = "updated"), "compared")[["cols"]], 2)
  expect_error(
    kompare(l, r, by = "id", ignore = c("id", "updated")),
    "ignore must not include by columns: id"
  )

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

test_that("digits must be a single finite integer", {
  l <- tibble::tibble(id = 1, x = 1.2)

  expect_error(kompare(l, l, by = "id", digits = "4"), "digits must be")
  expect_error(kompare(l, l, by = "id", digits = NA), "digits must be")
  expect_error(kompare(l, l, by = "id", digits = Inf), "digits must be")
  expect_error(kompare(l, l, by = "id", digits = c(1, 2)), "digits must be")
  expect_error(kompare(l, l, by = "id", digits = TRUE), "digits must be")
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

test_that("ignore names columns that exist in at least one frame", {
  l <- tibble::tibble(id = 1:2, Updated = c(1, 2))
  r <- l |> mutate(Updated = Updated + 1)

  expect_error(kompare(l, r, by = "id", ignore = "nope"),
    "doesn't exist")
  expect_no_error(
    kompare(tibble::tibble(id = 1, x = 1), tibble::tibble(id = 1, y = 1), by = "id", ignore = "x")
  )
  expect_length(kompare(l, r, by = "id")$diffs, 1)
  expect_length(kompare(l, r, by = "id", ignore = "Updated")$diffs, 0)
})

test_that("ignore accepts bare column names", {
  l <- tibble::tibble(id = 1:2, price = c(1, 2))
  r <- l |> mutate(price = price + 1)
  cols <- "price"

  expect_length(kompare(l, r, by = "id")$diffs, 1)
  expect_length(kompare(l, r, by = "id", ignore = c(price))$diffs, 0)
  expect_length(kompare(l, r, by = "id", ignore = price)$diffs, 0)
  expect_length(kompare(l, r, by = "id", ignore = all_of(cols))$diffs, 0)
  expect_length(kompare(l, r, by = "id", ignore = c(Price))$diffs, 0)
  expect_length(kompare(l, r, by = "id", ignore = starts_with("pri"))$diffs, 0)
  expect_error(kompare(l, r, by = "id", ignore = c("price", NA)), "valid tidyselect")
  expect_error(kompare(l, r, by = "id", ignore = c(price, NA)), "valid tidyselect")
  expect_error(kompare(l, r, by = "id", ignore = NA), "valid tidyselect")
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

test_that("print requires a positive integer n", {
  out <- kompare(tibble::tibble(id = 1), tibble::tibble(id = 1), by = "id")

  expect_error(print(out, n = 0), "positive integer")
  expect_error(print(out, n = NA), "positive integer")
  expect_error(print(out, n = 2.5), "positive integer")
  expect_error(format(out, n = -1), "positive integer")
  expect_no_error(capture.output(print(out, n = 2)))
})

test_that("type predicates in ignore see real column types", {
  l <- tibble::tibble(id = c("a", "b"), n = c(1, 2), s = c("a", "b"))
  r <- l |> mutate(n = n + 1, s = c("a", "z"))

  out <- kompare(l, r, by = "id", ignore = dplyr::where(is.numeric))

  expect_equal(attr(out, "settings")$ignore, "n")
  expect_null(out$diffs$dbl)
  expect_equal(nrow(out$diffs$chr), 1)
})
