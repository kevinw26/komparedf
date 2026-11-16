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
  expect_equal(out$diffs$dbl |>
                 filter(col == "sepal.length") |>
                 pull(id), 1)
})

test_that("keys, roundDigits, and round work as aliases", {
  l <- iris_base |> mutate(sepal.length = sepal.length + 1e-6)
  r <- iris_base

  expect_equal(kompare(l, r, keys = "id"), kompare(l, r, by = "id"))
  expect_equal(length(kompare(l, r, by = "id", round = 4)$diffs), 0)
  expect_equal(length(kompare(l, r, by = "id", roundDigits = 8)$diffs), 1)
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

test_that("kompare compares list columns", {
  l <- tibble(id = 1:2, x = list(1:2, "a"))
  r <- tibble(id = 1:2, x = list(1:2, "b"))

  out <- kompare(l, r, by = "id")
  flagged <- out$diffs$list

  expect_equal(nrow(flagged), 1)
  expect_equal(flagged$id, 2)
  expect_equal(flagged$.l[[1]], "a")
  expect_equal(flagged$.r[[1]], "b")
  expect_true(as.logical(kompare(l, l, by = "id")))
})

test_that("matrix and array columns compare and print as row cells", {
  l <- tibble(id = 1:2, x = matrix(c(1, 2, 3, 4), 2))
  r <- tibble(id = 1:2, x = matrix(c(1, 9, 3, 4), 2))

  out <- kompare(l, r, by = "id")
  flagged <- out$diffs[["dbl[,2]"]]

  expect_equal(nrow(flagged), 1)
  expect_equal(flagged$id, 2)
  expect_true(as.logical(kompare(l, l, by = "id")))

  txt <- capture.output(print(out))
  expect_true(any(str_detect(txt, "<2, 4>")))
  expect_true(any(str_detect(txt, "<9, 4>")))

  a <- array(as.double(1:8), c(2, 2, 2))
  b <- a
  b[1, , 1] <- 9
  arr <- kompare(tibble(id = 1:2, x = a), tibble(id = 1:2, x = b), by = "id")

  expect_equal(nrow(arr$diffs[["dbl[,2,2]"]]), 1)

  d1 <- tibble(id = 1:2, x = tibble(y = 1:2, z = 3:4))
  d2 <- tibble(id = 1:2, x = tibble(y = 1:2, z = c(3, 5)))
  expect_equal(nrow(kompare(d1, d2, by = "id")$diffs[["tibble[,2]"]]), 1)

  # wide row cells are clipped rather than printed in full
  wide <- matrix(1, 2, 10)
  wide_r <- wide
  wide_r[2,] <- 2
  wide_txt <- capture.output(print(kompare(
    tibble(id = 1:2, x = wide), tibble(id = 1:2, x = wide_r),
    by = "id"
  )))
  expect_true(any(str_detect(wide_txt, "<1, 1, 1, 1, 1, 1, …>")))
})

test_that("by rejects names reserved by the result schema", {
  expect_error(kompare(tibble(col = 1), tibble(col = 1), by = "col"), "reserved column names")
  expect_error(kompare(tibble(.l = 1), tibble(.l = 1), by = ".l"), "reserved column names")
  expect_error(kompare(tibble(.r = 1), tibble(.r = 1), by = ".r"), "reserved column names")
})

test_that("a type key does not clash with the diff type label", {
  l <- tibble(type = 1:2, x = c(1, 2))

  expect_no_error(capture.output(print(kompare(l, l, by = "type"))))

  out <- kompare(l, tibble(type = 1:2, x = c(9, 2)), by = "type")
  txt <- capture.output(print(out))

  expect_true(any(str_detect(txt, "data_type")))
  expect_true(any(str_detect(txt, "dbl")))

  l2 <- tibble(type = 1:2, data_type = c("a", "b"), x = c(1, 2))
  out2 <- kompare(l2, l2 |> mutate(x = c(9, 2)), by = c("type", "data_type"))
  expect_true(any(str_detect(capture.output(print(out2)), "\\.data_type")))
})

test_that("all-NA datetime columns are not coerced to date", {
  l <- tibble(id = 1:2, dt = as.POSIXct(c(NA, NA)))
  r <- tibble(id = 1:2, dt = as.POSIXct(c("2020-01-01 12:00:00", NA)))

  out <- kompare(l, r, by = "id")

  expect_equal(nrow(out$schema), 0)
  expect_equal(nrow(out$diffs$dttm), 1)
})

test_that("midnight on one side does not hide a time component on the other", {
  l <- tibble(id = 1, dt = as.POSIXct("2020-01-01 00:00:00", tz = "UTC"))
  r <- tibble(id = 1, dt = as.POSIXct("2020-01-01 12:00:00", tz = "UTC"))

  value_out <- suppressMessages(kompare(l, r, by = "id"))
  key_out <- suppressMessages(kompare(l, r, by = "dt"))

  expect_equal(nrow(value_out$diffs$dttm), 1)
  expect_equal(nrow(key_out$keys), 2)
})
