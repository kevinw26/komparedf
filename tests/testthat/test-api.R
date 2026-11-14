test_that("is_equal mirrors as.logical", {
  l <- tibble::tibble(id = 1:2, x = c(1, 2))
  r <- tibble::tibble(id = 1:2, x = c(1, 3))

  same <- kompare(l, l, by = "id")
  diff <- kompare(l, r, by = "id")

  expect_true(is_equal(same))
  expect_false(is_equal(diff))
  expect_identical(is_equal(diff), as.logical(diff))
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
  expect_s3_class(cnd$summary, "kompare")
  expect_match(rlang::cnd_message(cnd), "differences found")
})

test_that("format returns lines without printing", {
  l <- tibble::tibble(id = 1:2, x = c(1, 2))
  txt <- format(kompare(l, l, by = "id"))

  expect_type(txt, "character")
  expect_true(any(str_detect(txt, "^# kompare: 2 x 2 vs 2 x 2, by: id$")))
})

test_that("format echoes non-default settings", {
  l <- tibble::tibble(id = 1:2, x = c(1, 2))
  txt <- format(kompare(l, l, by = "id", digits = 8))

  expect_true(any(str_detect(txt, "# settings: digits = 8")))
})

test_that("n controls how many columns are shown", {
  l <- tibble::tibble(
    id = 1:2,
    v1 = c(1, 2), v2 = c(1, 2), v3 = c(1, 2), v4 = c(1, 2), v5 = c(1, 2)
  )
  r <- l |> dplyr::mutate(dplyr::across(-id, \(x) x + 1))

  txt <- format(kompare(l, r, by = "id"), n = 2)

  expect_true(any(str_detect(txt, "and 3 more columns")))
})

test_that("tidy gives per-column value counts", {
  l <- tibble::tibble(id = 1:3, x = c(1, 2, 3), y = c("a", "b", "c"))
  r <- tibble::tibble(id = 1:4, x = c(1, 9, 3, 4), y = c("a", "B", "c", "d"))

  out <- generics::tidy(kompare(l, r, by = "id"))

  expect_equal(out$type, c("dbl", "chr"))
  expect_equal(out$col, c("x", "y"))
  expect_equal(out$diffs, c(1L, 1L))
})
