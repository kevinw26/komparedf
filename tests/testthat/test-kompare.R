# tests/testthat/test-kompare.R

suppressPackageStartupMessages({
  library(testthat)
  library(dplyr)
  library(stringr)
  library(lubridate)
  library(purrr)
  library(tidyr)
  library(glue)
})

# shared base data set: iris with lowercase names and a unique id key
iris_base <- iris |>
  as_tibble() |>
  rename_with(str_to_lower) |>
  mutate(id = row_number())

test_that("omission of join_by defaults to positional comparison", {
  l <- tibble::tibble(x = c(12, 22))
  r <- l |> mutate(x = c(14, 24))

  # digits = 0 rounds to nearest 1, so 12 != 14
  expect_length(kompare(l, r, digits = 0)$diffs, 1)
  
  # digits = -1 rounds to nearest 10, so 12 == 10 and 14 == 10
  expect_length(kompare(l, r, digits = -1)$diffs, 0)
})

test_that("kompare flags a numeric value shifted by adding 1", {
  l <- iris_base
  r <- iris_base |> mutate(sepal.length = if_else(id == 1, sepal.length + 1, sepal.length))

  out <- kompare(l, r, keys = "id")
  flagged <- out$diffs$dbl |> filter(col == "sepal.length")

  expect_equal(nrow(flagged), 1)
  expect_equal(flagged |> pull(id), 1)
})

test_that("kompare flags a value set to na", {
  l <- iris_base
  r <- iris_base |> mutate(sepal.width = if_else(id == 5, NA_real_, sepal.width))

  out <- kompare(l, r, keys = "id")
  flagged <- out$diffs$dbl |> filter(col == "sepal.width")

  expect_equal(nrow(flagged), 1)
  expect_equal(flagged |> pull(id), 5)
})

test_that("kompare flags a column present in only one data set", {
  l <- iris_base |> mutate(rand_int = sample(1:100, n(), replace = TRUE))
  r <- iris_base

  out <- kompare(l, r, keys = "id")

  expect_true("rand_int" %in% (out$schema |> pull(col)))
})

test_that("kompare flags a date difference", {
  n <- nrow(iris_base)
  obs_dt <- as_date("2010-01-01") + sample(0:364, n, replace = TRUE)

  l <- iris_base |> mutate(obs_dt = as.POSIXct.Date(obs_dt))
  r <- l |> mutate(
    obs_dt = as_date(obs_dt),
    obs_dt = if_else(id == 10, obs_dt + 1, obs_dt)
  )

  out <- kompare(l, r, keys = "id")
  flagged <- out$diffs$date

  expect_equal(nrow(flagged), 1)
  expect_equal(flagged |> pull(id), 10)
})

test_that("kompare flags a string difference", {
  l <- iris_base |> mutate(species = as.character(species))
  r <- l |> mutate(species = if_else(id == 20, "unknown", species))

  out <- kompare(l, r, keys = "id")
  flagged <- out$diffs$chr

  expect_equal(nrow(flagged), 1)
  expect_equal(flagged |> pull(id), 20)
})

test_that("kompare does not flag a string difference", {
  l <- iris_base |> mutate(species = as.character(species))
  r <- l |> mutate(species = if_else(id == 20, "sEtOsA ", species))

  out <- kompare(l, r, keys = "id", strings = "ignorecase_trim")
  flagged <- out$diffs

  expect_null(flagged$diffs$chr)
})

test_that("kompare flags unmatched keys", {
  l <- iris_base
  r <- iris_base |> filter(!between(id, 3, 15))

  out <- kompare(l, r, keys = join_by(id))

  expect_equal(nrow(out$keys), 13)
  expect_equal(out$keys |> pull(id), 3:15)
})

test_that("kompare rejects non-data-frame inputs", {
  expect_error(kompare(1:3, iris_base), "l must be a data frame")
  expect_error(kompare(iris_base, 1:3), "r must be a data frame")
})

test_that("validate_frame rejects non-data-frames", {
  expect_error(validate_frame(1:3, "l"), "l must be a data frame")
  expect_error(validate_frame(NULL, "r"), "r must be a data frame")
})

test_that("validate_frame rejects duplicate names created by lowercasing", {
  dupes <- tibble(Id = 1, id = 2, .name_repair = "minimal")

  expect_error(validate_frame(dupes, "l"), "l has duplicate column names")
  expect_error(kompare(dupes, dupes), "duplicate column names")
})

test_that("validate_frame returns the frame with lower-case names", {
  out <- validate_frame(tibble(Id = 1:3, Value = c("a", "b", "c")), "l")

  expect_equal(names(out), c("id", "value"))
  expect_equal(nrow(out), 3)
})

test_that("kompare messages once for all date columns with no time component", {
  l <- iris_base |>
    mutate(
      obs_dt = as.POSIXct.Date(as_date("2010-01-01") + 0:(n() - 1)),
      upd_dt = as.POSIXct.Date(as_date("2011-01-01") + 0:(n() - 1))
    )
  r <- l |> mutate(obs_dt = obs_dt + 86400)

  msgs <- capture_messages(kompare(l, r, keys = "id"))

  expect_length(msgs, 1)
  expect_match(msgs, "obs_dt, upd_dt")
})

test_that("by accepts a named vector for asymmetric column names", {
  l <- iris_base
  r <- iris_base |>
    rename(ident = id) |>
    mutate(sepal.length = if_else(ident == 1, sepal.length + 1, sepal.length))

  out <- kompare(l, r, by = c(id = "ident"))

  expect_equal(nrow(out$keys), 0)
  expect_equal(out$diffs$dbl |> filter(col == "sepal.length") |> pull(id), 1)
})

test_that("keys, roundDigits, and round work as aliases", {
  l <- iris_base |> mutate(sepal.length = sepal.length + 1e-6)
  r <- iris_base

  expect_equal(kompare(l, r, keys = "id"), kompare(l, r, by = "id"))
  expect_equal(length(kompare(l, r, by = "id", round = 4)$diffs), 0)
  expect_equal(length(kompare(l, r, by = "id", roundDigits = 8)$diffs), 1)
})

test_that("print reports compared cells and all match", {
  same <- kompare(iris_base, iris_base, by = "id")
  txt <- capture.output(print(same))

  expect_true(any(str_detect(txt, "compared 150 rows and 6 columns")))
  expect_true(any(str_detect(txt, "all match")))
})

test_that("print counts differences", {
  l <- tibble(id = 1:2, x = c(1, 2))
  r <- l |> mutate(x = x + 1)

  txt <- capture.output(print(kompare(l, r, by = "id")))

  expect_true(any(str_detect(txt, "2 differences found")))
})

test_that("print notes different input lengths", {
  l <- tibble(id = 1:3, x = 1:3)
  r <- tibble(id = 1:2, x = 1:2)

  txt <- capture.output(print(kompare(l, r, by = "id")))

  expect_true(any(str_detect(txt, "different lengths \\(3 vs 2\\)")))
})

test_that("check_cells warns when a comparison is large", {
  l <- tibble(x = 1:10)
  r <- tibble(x = 1:10)

  expect_warning(check_cells(l, r, c("a", "b"), limit = 10), "cells to compare")
  expect_no_warning(check_cells(l, r, "a", limit = 100))
  expect_equal(check_cells(l, r, c("a", "b")), 20)
})

test_that("kompare rejects a by spec that repeats a column", {
  expect_error(kompare(iris_base, iris_base, by = c("id", "id")), "by repeats a column")
  expect_error(kompare(iris_base, iris_base, by = c("id", "ID")), "by repeats a column")
  expect_error(kompare(iris_base, iris_base, by = c(id = "ident", id = "ident2")), "by repeats a column")
  expect_error(kompare(iris_base, iris_base, by = c(id = "ident", grp = "ident")), "by repeats a column")
})

test_that("strings and factors are checked against their choices", {
  expect_error(kompare(iris_base, iris_base, strings = "tight"), "combination of ignorecase and trim")
  expect_error(kompare(iris_base, iris_base, factors = "character"), "must be one of")
})
