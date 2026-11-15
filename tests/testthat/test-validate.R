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

test_that("repeated by columns say which side", {
  expect_error(
    kompare(tibble::tibble(id = 1), tibble::tibble(id = 1), by = c(id = "a", id = "b")),
    "on the l side"
  )
})

test_that("grouped frames are compared ungrouped", {
  l <- dplyr::group_by(tibble::as_tibble(iris), Species)
  r <- dplyr::group_by(tibble::as_tibble(iris), Species)

  out <- kompare(l, r)

  expect_true(as.logical(out))
  expect_false(dplyr::is_grouped_df(validate_frame(l, "l")))
})

test_that("grouped frames produce no grouping messages", {
  l <- dplyr::group_by(tibble::as_tibble(iris), Species)

  expect_no_message(kompare(l, l))
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
  expect_s3_class(out, "tbl_df")
})

test_that("base data frames are converted to tibbles", {
  out <- validate_frame(data.frame(Id = 1:2, x = 1:2), "l")

  expect_s3_class(out, "tbl_df")
  expect_equal(names(out), c("id", "x"))
})

test_that("data tables warn and are converted to tibbles", {
  dt <- structure(data.frame(id = 1:2, x = 1:2), class = c("data.table", "data.frame"))

  expect_warning(out <- validate_frame(dt, "l"), "l is a data table")
  expect_s3_class(out, "tbl_df")
})

test_that("base data frames compare with a single key", {
  l <- data.frame(id = 1:2, x = c(1, 2))
  r <- data.frame(id = 1:2, x = c(1, 3))

  out <- kompare(l, r, by = "id")

  expect_equal(nrow(out$diffs$dbl), 1)
  expect_true(as.logical(kompare(l, l, by = "id")))
  expect_true(as.logical(kompare(l, l)))
})

test_that("validate_frame rejects missing and empty names", {
  na_name <- data.frame(id = 1, x = 2)
  names(na_name) <- c("id", NA)
  empty_name <- data.frame(id = 1, x = 2)
  names(empty_name) <- c("id", "")

  expect_error(validate_frame(na_name, "l"), "l has missing column names")
  expect_error(validate_frame(empty_name, "r"), "r has empty column names")
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

test_that("reserved column names are rejected", {
  expect_error(
    kompare(tibble::tibble(.rn = 1, x = 1), tibble::tibble(.rn = 1, x = 1), by = "x"),
    "reserved column name"
  )
  expect_error(
    kompare(tibble::tibble(id = 1, "a\x1fb" = 1), tibble::tibble(id = 1), by = "id"),
    "unit separator"
  )
})

test_that("by columns must exist on the correct side", {
  expect_error(kompare(tibble::tibble(id = 1), tibble::tibble(x = 1), by = "id"),
               "missing from r: id")
  expect_error(kompare(tibble::tibble(x = 1), tibble::tibble(id = 1), by = "id"),
               "missing from l: id")
})

test_that("by must use equality conditions only", {
  l <- tibble::tibble(a = 1:2)
  r <- tibble::tibble(b = 1:2)
  bc <- tibble::tibble(b = 1, c = 2)

  expect_error(kompare(l, r, by = join_by(a >= b)), "equality conditions")
  expect_error(kompare(l, r, by = join_by(closest(a >= b))), "equality conditions")
  expect_error(kompare(tibble::tibble(a = 1), bc, by = join_by(between(a, b, c))),
               "equality conditions")
})

test_that("by columns must have compatible types", {
  # character vs double
  expect_error(
    kompare(tibble::tibble(id = "1"), tibble::tibble(id = 1), by = "id"),
    "compatible types in l and r: id"
  )

  # report only the offending key
  expect_error(
    kompare(
      tibble::tibble(id = 1L, grp = "a"),
      tibble::tibble(id = 1, grp = 1),
      by = c("id", "grp")
    ),
    "compatible types in l and r: grp"
  )

  # integer vs double
  expect_true(as.logical(
    kompare(tibble::tibble(id = 1L), tibble::tibble(id = 1), by = "id")
  ))

  # factor vs character
  expect_true(as.logical(
    kompare(tibble::tibble(id = factor("a")), tibble::tibble(id = "a"), by = "id")
  ))
})

test_that("by rejects unsupported specifications", {
  l <- tibble::tibble(id = 1)

  expect_error(kompare(l, l, by = 1), "must be NULL")
  expect_error(kompare(l, l, by = ~id), "must be NULL")
  expect_error(kompare(l, l, by = character()), "at least one column")
  expect_error(kompare(l, l, by = c("id", x = "id")), "all be given or all omitted")
})

test_that("asymmetric by rejects renames that would collide", {
  expect_error(
    kompare(tibble::tibble(id = 1), tibble::tibble(id = 1, ident = 1), by = c(id = "ident")),
    "already has those columns"
  )
  expect_error(
    kompare(tibble::tibble(a = 1), tibble::tibble(a = 1, b = 1), by = join_by(a == b)),
    "already has those columns"
  )
})

test_that("dots reject unknown names and alias conflicts", {
  l <- tibble::tibble(id = 1, x = 1)

  expect_error(kompare(l, l, keyz = "id"), "must be empty")
  expect_error(kompare(l, l, by = "id", keys = "id"), "Exactly one of")
  expect_error(kompare(l, l, by = "id", digits = 2, round = 2), "only one of digits")
  expect_error(kompare(l, l, by = "id", round = 2, roundDigits = 2), "Exactly one of")
})

test_that("fn output is re-validated for emptiness", {
  l <- tibble::tibble(id = 1, x = 1)

  expect_error(kompare(l, l, fn = \(d) d[0,]), "must not be empty")
  expect_error(kompare(l, l, fn = \(d) d[, 0]), "at least one column")
})

test_that("positional comparison does not count the row number as a column", {
  l <- tibble::tibble(a = 1:2, b = 3:4)

  expect_equal(attr(kompare(l, l), "compared")[["cols"]], 2)
})

test_that("by rejects missing and empty column names", {
  l <- tibble::tibble(id = 1:2)

  expect_error(kompare(l, l, by = NA_character_), "must not be NA or empty")
  expect_error(kompare(l, l, by = ""), "must not be NA or empty")
  expect_error(kompare(l, l, by = c("id", NA)), "must not be NA or empty")
})

test_that("by columns must be atomic", {
  l <- tibble::tibble(id = list(1, 2), x = 1:2)

  expect_error(kompare(l, l, by = "id"), "by columns must be atomic")
})
