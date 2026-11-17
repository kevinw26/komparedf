test_that("summary prints the report and returns the object invisibly", {
  l <- tibble::tibble(id = 1:2, x = c(1, 2))
  r <- tibble::tibble(id = 1:2, x = c(2, 3))
  out <- kompare(l, r, by = "id")

  txt <- capture.output(res <- summary(out))

  expect_identical(res, out)
  expect_true(any(str_detect(txt, "2 differences found")))
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

test_that("kompare prints integer64 differences exactly", {
  skip_if_not_installed("bit64")

  l <- tibble::tibble(id = 1, x = bit64::as.integer64("9007199254740993"))
  r <- tibble::tibble(id = 1, x = bit64::as.integer64("9007199254740995"))

  expect_no_warning(txt <- format(kompare(l, r, by = "id")))
  expect_true(any(str_detect(txt, "9007199254740993")))
  expect_true(any(str_detect(txt, "9007199254740995")))
})

test_that("print reports compared cells", {
  same <- kompare(
    tibble::tibble(id = 1:150, a = 1, b = 1, c = 1, d = 1, e = 1),
    tibble::tibble(id = 1:150, a = 1, b = 1, c = 1, d = 1, e = 1),
    by = "id"
  )
  txt <- capture.output(print(same))

  expect_true(any(str_detect(txt, "compared 150 rows and 6 columns")))
})

test_that("print notes different input lengths", {
  l <- tibble::tibble(id = 1:3, x = 1:3)
  r <- tibble::tibble(id = 1:2, x = 1:2)

  txt <- capture.output(print(kompare(l, r, by = "id")))

  expect_true(any(str_detect(txt, "different lengths \\(3 vs 2\\)")))
})

test_that("print handles differences of several types", {
  l <- tibble::tibble(id = 1:2, x = c(1, 2), s = c("a", "b"))
  r <- tibble::tibble(id = 1:2, x = c(1, 9), s = c("a", "z"))

  txt <- capture.output(print(kompare(l, r, by = "id")))

  expect_true(any(str_detect(txt, "dbl")))
  expect_true(any(str_detect(txt, "chr")))
})

test_that("print requires a positive integer n", {
  out <- kompare(tibble::tibble(id = 1), tibble::tibble(id = 1), by = "id")

  expect_error(print(out, n = 0), "positive integer")
  expect_error(print(out, n = NA), "positive integer")
  expect_error(print(out, n = 2.5), "positive integer")
  expect_error(format(out, n = -1), "positive integer")
  expect_no_error(capture.output(print(out, n = 2)))
})

test_that("methods reject unused dots", {
  out <- kompare(tibble::tibble(id = 1), tibble::tibble(id = 1), by = "id")

  expect_error(print(out, foo = 1), "must be empty")
  expect_error(summary(out, foo = 1), "must be empty")
  expect_error(format(out, foo = 1), "must be empty")
  expect_error(generics::tidy(out, foo = 1), "must be empty")
  expect_error(as.logical(out, foo = 1), "must be empty")
})

test_that("print caps unbounded n", {
  out <- kompare(tibble::tibble(id = 1), tibble::tibble(id = 1), by = "id")

  expect_no_warning(format(out, n = Inf))
  expect_no_warning(format(out, n = 2^31))
})
