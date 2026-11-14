suppressPackageStartupMessages(library(dplyr))

test_that("tidyr table1 survives an openxlsx round trip", {
  skip_if_not_installed("openxlsx")

  df <- tidyr::table1
  path <- withr::local_tempfile(fileext = ".xlsx")
  openxlsx::write.xlsx(df, path)
  out <- openxlsx::read.xlsx(path)

  expect_true(as.logical(kompare(df, out, by = c("country", "year"))))
})

test_that("tidyr table2 survives a writexl round trip", {
  skip_if_not_installed("writexl")
  skip_if_not_installed("openxlsx")

  df <- tidyr::table2
  path <- withr::local_tempfile(fileext = ".xlsx")
  writexl::write_xlsx(df, path)
  out <- openxlsx::read.xlsx(path, detectDates = TRUE)

  expect_true(as.logical(kompare(df, out, by = c("country", "year", "type"))))
})

test_that("dates survive an openxlsx round trip", {
  skip_if_not_installed("openxlsx")

  df <- tidyr::billboard |> select(artist, track, date.entered)
  path <- withr::local_tempfile(fileext = ".xlsx")
  openxlsx::write.xlsx(df, path)
  out <- openxlsx::read.xlsx(path, detectDates = TRUE)

  expect_true(as.logical(kompare(df, out, by = c("artist", "track"))))
})
