suppressPackageStartupMessages(library(dplyr))

test_that("integer64 columns are rounded like round()", {
  skip_if_not_installed("bit64")

  l <- tibble::tibble(id = 1:3, x = bit64::as.integer64(c(1, 5, 11)))
  r <- tibble::tibble(id = 1:3, x = bit64::as.integer64(c(5, 6, 14)))

  flagged <- kompare(l, r, by = "id", digits = -1)$diffs$int64

  expect_equal(flagged$id, 2)
})

test_that("kompare compares integer64 columns exactly", {
  skip_if_not_installed("bit64")

  l <- tibble::tibble(
    id = 1:3,
    notional = bit64::as.integer64(c("9007199254740993", "2", "3"))
  )
  r <- l
  r$notional[2] <- bit64::as.integer64("9007199254740995")

  out <- kompare(l, r, by = "id")
  flagged <- out$diffs$int64

  expect_equal(nrow(flagged), 1)
  expect_equal(flagged$id, 2)
  expect_equal(as.character(flagged$.r), "9007199254740995")
})

test_that("kompare treats equal integer64 columns as matching", {
  skip_if_not_installed("bit64")

  d <- tibble::tibble(
    id = 1:2,
    notional = bit64::as.integer64(c("1", "9223372036854775807"))
  )

  expect_true(as.logical(kompare(d, d, by = "id")))
})

test_that("kompare matches integer64 keys", {
  skip_if_not_installed("bit64")

  l <- tibble::tibble(
    id = bit64::as.integer64(c("1", "9223372036854775807")),
    v = c(1, 2)
  )
  r <- l |> filter(row_number() == 1)

  out <- kompare(l, r, by = "id")

  expect_equal(nrow(out$keys), 1)
  expect_equal(as.character(out$keys$id), "9223372036854775807")
})
