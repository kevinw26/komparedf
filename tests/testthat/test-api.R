test_that("as.logical reports equality", {
  l <- tibble::tibble(id = 1:2, x = c(1, 2))
  r <- tibble::tibble(id = 1:2, x = c(1, 3))

  same <- kompare(l, l, by = "id")
  diff <- kompare(l, r, by = "id")

  expect_true(as.logical(same))
  expect_false(as.logical(diff))
})

test_that("summary prints the report and returns the object invisibly", {
  l <- tibble::tibble(id = 1:2, x = c(1, 2))
  r <- tibble::tibble(id = 1:2, x = c(2, 3))
  out <- kompare(l, r, by = "id")

  txt <- capture.output(res <- summary(out))

  expect_identical(res, out)
  expect_true(any(str_detect(txt, "differences found")))
})

test_that("raise_error attaches the summary to the condition", {
  l <- tibble::tibble(id = 1:2, x = c(1, 2))
  r <- tibble::tibble(id = 1:2, x = c(2, 3))

  cnd <- rlang::catch_cnd(kompare(l, r, by = "id", raise_error = TRUE))

  expect_s3_class(cnd, "kompare_error")
  expect_s3_class(cnd$obj, "kompare")
  expect_match(rlang::cnd_message(cnd), "differences found")
})

test_that("format returns lines without printing", {
  l <- tibble::tibble(id = 1:2, x = c(1, 2))
  txt <- format(kompare(l, l, by = "id"))

  expect_type(txt, "character")
  expect_true(any(str_detect(txt, "all match")))
})

test_that("format preserves dates and datetimes", {
  l <- tibble::tibble(
    id = 1,
    date = as.Date("2020-01-01"),
    time = as.POSIXct("2020-01-01 01:00:00", tz = "UTC")
  )
  r <- tibble::tibble(
    id = 1,
    date = as.Date("2020-01-02"),
    time = as.POSIXct("2020-01-01 02:00:00", tz = "UTC")
  )

  expect_no_error(txt <- format(kompare(l, r, by = "id")))
  expect_true(any(str_detect(txt, "2020-01-02")))
  expect_true(any(str_detect(txt, "01:00:00")))
  expect_true(any(str_detect(txt, "02:00:00")))
})

test_that("tidy gives per-column value counts", {
  l <- tibble::tibble(id = 1:3, x = c(1, 2, 3), y = c("a", "b", "c"))
  r <- tibble::tibble(id = 1:4, x = c(1, 9, 3, 4), y = c("a", "B", "c", "d"))

  out <- generics::tidy(kompare(l, r, by = "id"))

  expect_equal(out$type, c("dbl", "chr"))
  expect_equal(out$col, c("x", "y"))
  expect_equal(out$diffs, c(1L, 1L))
  expect_equal(out$n, c(3L, 3L))
})
