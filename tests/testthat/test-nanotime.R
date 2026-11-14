test_that("kompare compares nanotime values down to the nanosecond", {
  skip_if_not_installed("nanotime")

  l <- tibble::tibble(
    id = 1:3,
    ts = nanotime::nanotime(c(
      "2024-01-02T09:30:00.000000001+00:00",
      "2024-01-02T09:31:00+00:00",
      "2024-01-02T09:32:00+00:00"
    ))
  )
  r <- l
  r$ts[2] <- r$ts[2] + 1

  out <- kompare(l, r, by = "id")
  flagged <- out$diffs$nanotime

  expect_equal(nrow(flagged), 1)
  expect_equal(flagged$id, 2)
  expect_equal(flagged$.l, l$ts[2])
  expect_equal(flagged$.r, r$ts[2])
})

test_that("kompare treats equal nanotime columns as matching", {
  skip_if_not_installed("nanotime")

  d <- tibble::tibble(
    id = 1:2,
    ts = nanotime::nanotime(c(
      "2024-01-02T09:30:00+00:00",
      "2024-01-02T09:31:00.123456789+00:00"
    ))
  )

  expect_true(as.logical(kompare(d, d, by = "id")))
})

test_that("kompare matches nanotime keys", {
  skip_if_not_installed("nanotime")

  l <- tibble::tibble(
    ts = nanotime::nanotime(c("2024-01-02T09:30:00+00:00", "2024-01-02T09:31:00+00:00")),
    v = c(1, 2)
  )
  r <- l |> dplyr::slice(1)

  out <- kompare(l, r, by = "ts")

  expect_equal(nrow(out$keys), 1)
  expect_length(out$diffs, 0)
})
