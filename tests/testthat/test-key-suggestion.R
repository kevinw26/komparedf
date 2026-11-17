test_that("key candidates exclude unusable columns", {
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
  cols <- c("a", "lst", "bad")

  valid <- is_atomic_pair(l, r, cols) &
    purrr::map_lgl(cols, \(col) has_common_dtype(l[[col]], r[[col]]))

  expect_equal(valid, c(TRUE, FALSE, FALSE))
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
  expect_error(
    suggest_kompare_keys(l, l, by = c("id", "id")),
    "by repeats a column"
  )

  list_col <- tibble::tibble(id = list(1, 2), x = 1:2)
  expect_error(
    suggest_kompare_keys(list_col, list_col, by = "id"),
    "by columns must be atomic"
  )

  expect_error(
    suggest_kompare_keys(l, l, by = "id", max_add = NA),
    "max_add must be a single non-negative integer"
  )
  expect_error(
    suggest_kompare_keys(l, l, by = "id", max_add = -1),
    "max_add must be a single non-negative integer"
  )
  expect_error(
    suggest_kompare_keys(l, l, by = "id", max_add = 2.5),
    "max_add must be a single non-negative integer"
  )
  expect_error(
    suggest_kompare_keys(l, l, by = "id", max_candidates = NA),
    "max_candidates must be a single non-negative integer"
  )
})

test_that("suggestion normalises names like kompare", {
  l <- tibble::tibble(Id = 1:2)
  r <- tibble::tibble(id = 1:2)

  key <- suggest_kompare_keys(l, r)

  expect_equal(key, "id")
  expect_true(as.logical(kompare(l, r, by = key)))
  expect_true(as.logical(kompare(l, r, by = dplyr::join_by(id))))
  expect_equal(suggest_kompare_keys(l, r, by = "ID"), "id")
  expect_equal(suggest_kompare_keys(l, r, by = c(Id = "id")), "id")

  dupes <- tibble::tibble(Id = 1:2, id = 2:1, .name_repair = "minimal")
  expect_error(
    suggest_kompare_keys(dupes, dupes),
    "duplicate column names after lowercasing"
  )
})

test_that("suggestion never returns reserved result columns", {
  for (key in c("col", ".l", ".r")) {
    d <- tibble::tibble(!!key := 1:2, id = c("a", "b"))

    expect_equal(suggest_kompare_keys(d, d), "id")
    expect_error(suggest_kompare_keys(d, d, by = key), "reserved column names")
  }

  expect_error(
    suggest_kompare_keys(tibble::tibble(.L = 1:2), tibble::tibble(.L = 1:2), by = ".L"),
    "reserved column names"
  )
  expect_error(
    suggest_kompare_keys(tibble::tibble(col = 1:2), tibble::tibble(col = 1:2)),
    "no obvious unique key found"
  )
})

test_that("suggestion caps unbounded search limits", {
  d <- tibble::tibble(id = 1:2)

  expect_no_warning(
    expect_equal(
      suggest_kompare_keys(d, d, by = "id", max_add = Inf, max_candidates = Inf),
      "id"
    )
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
