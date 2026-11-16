suppressPackageStartupMessages(library(dplyr))

test_that("digits sets the numeric tolerance", {
  l <- tibble::tibble(id = 1:2, x = c(1.4, 2.4))
  r <- l |> mutate(x = x + 1e-6)
  near <- l |> mutate(x = x + 0.4)
  far <- l |> mutate(x = x + 0.6)

  expect_length(kompare(l, r, by = "id")$diffs, 0)
  expect_length(kompare(l, r, by = "id", digits = 8)$diffs, 1)
  # half a unit in the last place: 0.5 at digits = 0
  expect_length(kompare(l, near, by = "id", digits = 0)$diffs, 0)
  expect_length(kompare(l, far, by = "id", digits = 0)$diffs, 1)
  expect_length(kompare(l, near, by = "id", digits = -1)$diffs, 0)
})

test_that("digits is bounded to the range where rounding is meaningful", {
  l <- tibble::tibble(id = 1, x = 1)
  r <- tibble::tibble(id = 1, x = 9)

  # would coerce to NA and silently match every numeric column
  expect_error(kompare(l, r, by = "id", digits = 1e10), "between -22 and 22")
  expect_error(kompare(l, r, by = "id", digits = 23), "between -22 and 22")
  expect_error(kompare(l, r, by = "id", digits = -23), "between -22 and 22")
})

test_that("NULL digits disables rounding", {
  l <- tibble::tibble(id = 1, x = 0.1 + 0.2)
  r <- tibble::tibble(id = 1, x = 0.3)

  expect_length(kompare(l, r, by = "id")$diffs, 0)
  expect_length(kompare(l, r, by = "id", digits = NULL)$diffs, 1)
  expect_length(kompare(l, r, by = "id", round = NULL)$diffs, 1)
  expect_length(kompare(l, r, by = "id", roundDigits = NULL)$diffs, 1)
})

test_that("digits must be a single finite integer", {
  l <- tibble::tibble(id = 1, x = 1.2)

  expect_error(kompare(l, l, by = "id", digits = "4"), "digits must be")
  expect_error(kompare(l, l, by = "id", digits = NA), "digits must be")
  expect_error(kompare(l, l, by = "id", digits = Inf), "digits must be")
  expect_error(kompare(l, l, by = "id", digits = c(1, 2)), "digits must be")
  expect_error(kompare(l, l, by = "id", digits = TRUE), "digits must be")
})

test_that("values either side of a rounding midpoint match", {
  mid <- c(0.005, 0.015, 0.125, 0.995, 1.005, 12.345)
  l <- tibble::tibble(id = seq_along(mid), x = mid - 1e-11)
  r <- tibble::tibble(id = seq_along(mid), x = mid + 1e-11)

  # plain rounding splits every pair onto different sides
  expect_true(all(round(l$x, 2) != round(r$x, 2)))

  # tolerance keeps them together
  expect_length(kompare(l, r, by = "id", digits = 2)$diffs, 0)
})

test_that("binary error above half a unit is reported", {
  # a fixed ulp guard hid differences above tol once ulp approached tol, such
  # as 1e16 vs 1e16 + 2; the intended decimal value cannot be recovered from
  # the stored double, so the stored gap is compared to tol directly; ulp only
  # warns when representation is coarser than the requested tolerance
  l <- tibble::tibble(id = 1:2, x = c(0.125, 0.1))
  r <- tibble::tibble(id = 1:2, x = c(0.13, 0.105))

  flagged <- kompare(l, r, by = "id", digits = 2)$diffs$dbl
  expect_equal(flagged$id, 1)
})

test_that("representable differences are reported", {
  # ulp at 1e16 is 2, far above tol = 5e-5; warn because the requested
  # precision is below the data's resolution; the one-ulp gap is exactly
  # representable, so it is reported rather than absorbed by a guard
  l <- tibble::tibble(id = 1, x = 1e16)
  r <- tibble::tibble(id = 1, x = 1e16 + 2)

  expect_warning(
    out <- kompare(l, r, by = "id"),
    "floating point representation is less accurate"
  )
  expect_equal(nrow(out$diffs$dbl), 1)
})
