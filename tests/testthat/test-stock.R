suppressPackageStartupMessages(library(dplyr))

# minute bars for one session as a random walk
make_bars <- \(tz = "UTC", seed = 1) {
  set.seed(seed)
  minutes <- seq(
    as.POSIXct("2024-01-02 09:30:00", tz = tz),
    as.POSIXct("2024-01-02 16:00:00", tz = tz),
    by = "min"
  )
  tibble::tibble(
    ts = minutes,
    symbol = "ABC",
    price = round(100 + cumsum(rnorm(length(minutes))), 2)
  )
}

test_that("identical minute bars match", {
  bars <- make_bars()
  expect_true(as.logical(kompare(bars, bars, by = "ts")))
})

test_that("retagging the time zone does not create differences", {
  l <- make_bars("UTC")
  r <- l
  attr(r$ts, "tzone") <- "America/New_York"

  expect_true(as.logical(kompare(l, r, by = "ts")))
  expect_equal(attr(r$ts, "tzone"), "America/New_York")
})

test_that("same clock times in different zones do not match", {
  l <- make_bars("UTC")
  r <- make_bars("Asia/Tokyo")

  out <- kompare(l, r, by = "ts")

  expect_equal(nrow(out$keys), nrow(l) + nrow(r))
  expect_length(out$diffs, 0)
})

test_that("one shifted minute shows up as unmatched keys", {
  l <- make_bars()
  r <- l
  r$ts[10] <- r$ts[10] + 30

  out <- kompare(l, r, by = "ts")

  expect_equal(nrow(out$keys), 2)
  expect_length(out$diffs, 0)
})

test_that("price moves are compared within the rounding tolerance", {
  l <- make_bars()

  moved <- l
  moved$price[5] <- moved$price[5] + 0.01
  out <- kompare(l, moved, by = "ts")

  expect_equal(nrow(out$diffs$dbl), 1)
  expect_equal(out$diffs$dbl$ts, moved$ts[5])

  tiny <- l
  tiny$price[5] <- tiny$price[5] + 1e-6
  expect_length(kompare(l, tiny, by = "ts")$diffs, 0)
})

test_that("a missing minute only shows up as an unmatched key", {
  l <- make_bars()
  r <- filter(l, row_number() != 20)

  out <- kompare(l, r, by = "ts")

  expect_equal(nrow(out$keys), 1)
  expect_length(out$diffs, 0)
})
