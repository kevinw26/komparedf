test_that("as.logical reports equality", {
  l <- tibble::tibble(id = 1:2, x = c(1, 2))
  r <- tibble::tibble(id = 1:2, x = c(1, 3))

  same <- kompare(l, l, by = "id")
  diff <- kompare(l, r, by = "id")

  expect_true(as.logical(same))
  expect_false(as.logical(diff))
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
