test_that("validate_frame rejects things that are not data frames", {
  expect_error(validate_frame(1:3, "l"), "l must be a data frame")
  expect_error(validate_frame(NULL, "r"), "r must be a data frame")
  expect_error(validate_frame(matrix(1:4, 2), "l"), "l must be a data frame")
  expect_error(validate_frame(list(a = 1), "l"), "l must be a data frame")
  expect_error(validate_frame("x", "r"), "r must be a data frame")
})

test_that("validate_frame errors name what is wrong", {
  expect_error(validate_frame(1:3, "l"), "not integer")
  expect_error(validate_frame(matrix(1:4, 2), "r"), "not matrix")
  expect_error(
    validate_frame(tibble::tibble(Id = 1, id = 2, .name_repair = "minimal"), "l"),
    "duplicate column names after lowercasing: id"
  )
})

test_that("duplicate key errors show the repeated keys", {
  l <- tibble::tibble(id = c(1, 1, 2, 2), x = 1:4)
  r <- tibble::tibble(id = 1, x = 1)

  expect_error(kompare(l, r, by = "id"), "repeated keys: 1, 2")
})

test_that("repeated by columns say which side", {
  expect_error(
    kompare(tibble::tibble(id = 1), tibble::tibble(id = 1), by = c(id = "a", id = "b")),
    "on the l side"
  )
})

test_that("validate_frame raises rlang errors", {
  expect_error(validate_frame(1:3, "l"), class = "rlang_error")
  expect_error(validate_frame(tibble::tibble(Id = 1, id = 2, .name_repair = "minimal"), "l"),
    class = "rlang_error"
  )
})

test_that("validate_frame lower-cases names without touching values", {
  d <- tibble::tibble(Id = 1:2, Value = c("a", "b"))
  out <- validate_frame(d, "l")

  expect_equal(names(out), c("id", "value"))
  expect_equal(out$id, d$Id)
  expect_equal(out$value, d$Value)
})

test_that("validate_frame rejects names that collide after lowercasing", {
  d <- tibble::tibble(Id = 1, id = 2, .name_repair = "minimal")
  expect_error(validate_frame(d, "l"), "l has duplicate column names after lowercasing")
})

test_that("kompare validates both sides", {
  df <- tibble::as_tibble(iris)

  expect_error(kompare(1:3, df), "l must be a data frame")
  expect_error(kompare(df, 1:3), "r must be a data frame")
  expect_error(kompare(df, df, by = c("Species", "Species")), "by repeats a column")
})
