test_that("is_validly_atomic excludes unusable columns", {
  l <- tibble::tibble(
    id = 1:3,
    a = 1:3,
    b = c(1, 1, 2),
    const = 1,
    lst = list(1, 2, 3),
    bad = 1:3
  )
  r <- l |>
    dplyr::mutate(bad = as.character(bad))

  expect_equal(is_validly_atomic(l, r, c("a", "lst", "bad")), c(TRUE, FALSE, FALSE))
})

test_that("suggest_kompare_keys returns single and composite keys", {
  composite <- tibble::tibble(
    id = c(1, 1, 2, 2),
    grp = c("a", "b", "a", "b")
  )
  expect_equal(suggest_kompare_keys(composite, composite, by = "id"), c("id", "grp"))

  single <- tibble::tibble(id = c(1, 1), uuid = c("a", "b"))
  expect_equal(suggest_kompare_keys(single, single, by = "id"), "uuid")

  expect_error(
    suggest_kompare_keys(composite, composite, by = dplyr::join_by(id)),
    "not join_by"
  )
  expect_error(
    suggest_kompare_keys(composite, composite, by = c(id = "grp")),
    "key renaming is not supported"
  )

  none <- tibble::tibble(id = c(1, 1), grp = c("a", "a"))
  expect_error(
    suggest_kompare_keys(none, none, by = "id"),
    "no obvious unique key found in the top 20"
  )
})

test_that("suggest_kompare_keys validates inputs", {
  l <- tibble::tibble(id = 1)

  expect_error(suggest_kompare_keys(1:3, l, by = "id"), "l must be a data frame")
  expect_error(suggest_kompare_keys(l, 1:3, by = "id"), "r must be a data frame")
  expect_error(
    suggest_kompare_keys(l, l, by = "x"),
    "by columns missing from l: x"
  )
  expect_error(
    suggest_kompare_keys(l, tibble::tibble(x = 1), by = "id"),
    "by columns missing from r: id"
  )
  expect_error(
    suggest_kompare_keys(l, l, by = NA_character_),
    "by must not be NA or empty"
  )
  expect_error(
    suggest_kompare_keys(l, l, by = ""),
    "by must not be NA or empty"
  )
})

test_that("duplicate key errors note distinctness", {
  l <- tibble::tibble(id = c(1, 1, 2, 2), x = 1:4)
  r <- tibble::tibble(id = 1, x = 1)

  expect_error(kompare(l, r, by = "id"), "by id is not distinct in l")

  cnd <- tryCatch(kompare(l, r, by = "id"), error = identity)
  expect_s3_class(cnd, "kompare_dupes_error")
  expect_null(cnd$dupes)
})

test_that("duplicate key suggestions are omitted when unavailable", {
  l <- tibble::tibble(id = c(1, 1), grp = c("a", "a"))
  r <- tibble::tibble(id = 1, grp = "a")

  cnd <- tryCatch(kompare(l, r, by = "id"), error = identity)

  expect_false(grepl("consider", conditionMessage(cnd)))
})

test_that("duplicate key errors suggest a unique key", {
  l <- tibble::tibble(id = c(1, 1, 2, 2), grp = c("a", "b", "a", "b"))
  r <- tibble::tibble(id = 1:2, grp = c("a", "b"))

  expect_error(
    kompare(l, r, by = "id"),
    "consider join_by\\(id, grp\\)"
  )
})
