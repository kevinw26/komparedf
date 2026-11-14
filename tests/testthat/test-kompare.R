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

test_that("kompare flags a numeric value shifted by adding 1", {
  l <- iris_base
  r <- iris_base |> mutate(sepal.length = if_else(id == 1, sepal.length + 1, sepal.length))

  out <- kompare(l, r, keys = "id")
  flagged <- out$diffs$numeric |> filter(col == "sepal.length")

  expect_equal(nrow(flagged), 1)
  expect_equal(flagged |> pull(id), 1)
})

test_that("kompare flags a value set to na", {
  l <- iris_base
  r <- iris_base |> mutate(sepal.width = if_else(id == 5, NA_real_, sepal.width))

  out <- kompare(l, r, keys = "id")
  flagged <- out$diffs$numeric |> filter(col == "sepal.width")

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
  flagged <- out$diffs$Date

  expect_equal(nrow(flagged), 1)
  expect_equal(flagged |> pull(id), 10)
})

test_that("kompare flags a string difference", {
  l <- iris_base |> mutate(species = as.character(species))
  r <- l |> mutate(species = if_else(id == 20, "unknown", species))

  out <- kompare(l, r, keys = "id")
  flagged <- out$diffs$character

  expect_equal(nrow(flagged), 1)
  expect_equal(flagged |> pull(id), 20)
})

test_that("kompare does not flag a string difference", {
  l <- iris_base |> mutate(species = as.character(species))
  r <- l |> mutate(species = if_else(id == 20, "sEtOsA ", species))

  out <- kompare(l, r, keys = "id", .strict_strings = FALSE)
  flagged <- out$diffs

  expect_null(flagged$diffs$character)
})

test_that("kompare flags unmatched keys", {
  l <- iris_base
  r <- iris_base |> filter(!between(id, 3, 15))

  out <- kompare(l, r, keys = join_by(id))

  expect_equal(nrow(out$keys), 13)
  expect_equal(out$keys |> pull(id), 3:15)
})
