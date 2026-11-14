make_stock_master <- \(n_symbols = 10, n_dates = 100) {
  symbols <- sprintf("S%02d", seq_len(n_symbols))
  dates <- as.Date("2024-01-01") + seq_len(n_dates) - 1

  tidyr::expand_grid(symbol = symbols, date = dates) |>
    dplyr::mutate(
      permno = as.integer(factor(symbol, levels = symbols)),
      permco = permno + 1000L,
      cusip = sprintf("C%08d", permno),
      isin = sprintf("US%010d", permno),
      sedol = sprintf("S%06d", permno),
      figi = sprintf("BBG%09d", permno),
      ticker = symbol,
      exchange = dplyr::if_else(permno %% 2L == 0L, "XNAS", "XNYS"),
      share_class = dplyr::if_else(permno %% 3L == 0L, "A", "B"),
      country = dplyr::if_else(permno %% 4L == 0L, "US", "CA"),
      currency = dplyr::if_else(permno %% 5L == 0L, "USD", "CAD"),
      sector = dplyr::case_when(
        permno %% 3L == 0L ~ "tech",
        permno %% 3L == 1L ~ "finance",
        TRUE ~ "industrial"
      ),
      security_type = "common",
      notes = "x",
      metadata = rep(list(NULL), dplyr::n())
    )
}

test_that("suggest_kompare_keys handles a wide stock master", {
  master <- make_stock_master()

  expect_equal(
    suggest_kompare_keys(master, master, by = "symbol"),
    c("symbol", "date")
  )

  cnd <- tryCatch(kompare(master, master, by = "symbol"), error = identity)
  expect_match(conditionMessage(cnd), "consider join_by\\(symbol, date\\)")
})

test_that("suggest_kompare_keys ignores unusable identifier columns", {
  l <- make_stock_master()
  r <- l |>
    dplyr::mutate(mixed = as.character(permno))
  l <- l |>
    dplyr::mutate(mixed = permno)
  l$only_l <- seq_len(nrow(l))

  expect_equal(
    suggest_kompare_keys(l, r, by = "symbol"),
    c("symbol", "date")
  )
})

test_that("suggest_kompare_keys grows a five-column key", {
  d <- tidyr::expand_grid(
    a = 0:1,
    b = 0:1,
    c = 0:1,
    d = 0:1,
    e = 0:1
  ) |>
    dplyr::mutate(grp = "x")

  expect_equal(
    suggest_kompare_keys(d, d, by = "grp"),
    c("grp", "a", "b", "c", "d", "e")
  )
  expect_error(
    suggest_kompare_keys(d, d, by = "grp", max_add = 4),
    "no obvious unique key found"
  )
})

test_that("suggest_kompare_keys errors when rows are identical", {
  d <- tibble::tibble(
    symbol = c("A", "A"),
    date = as.Date(c("2024-01-01", "2024-01-01")),
    permno = c(1, 1)
  )

  expect_error(
    suggest_kompare_keys(d, d, by = "symbol"),
    "no obvious unique key found"
  )
})
