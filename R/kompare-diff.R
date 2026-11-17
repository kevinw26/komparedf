# vector type without dimensions
strip_dims <- \(x) {
  x <- vec_ptype(x)
  dim(x) <- NULL
  x
}

# values whose vctrs abbreviation needs rounding or tolerance
is_roundable_numeric <- \(x) {
  vec_ptype_abbr(strip_dims(x)) %in% c("int", "int64", "dbl")
}

# whether two vectors have a common vctrs prototype
has_common_dtype <- \(x, y) {
  try_fetch({
    vec_ptype_common(x, y)
    TRUE
  }, error = \(cnd) FALSE)
}

# unequal values, treating NaN as NA
neq <- \(x, y) {
  if (is.numeric(x) && is.numeric(y)) {
    x <- if (any(is.nan(x))) replace(x, is.nan(x), NA) else x
    y <- if (any(is.nan(y))) replace(y, is.nan(y), NA) else y
  }
  !vec_equal(x, y, na_equal = TRUE)
}

# ulp of a numeric vector, floored at the smallest positive double
numeric_ulp <- \(v) {
  v <- abs(as.numeric(v))
  pmax(2^(floor(log2(v)) - 52), .Machine$double.xmin * .Machine$double.eps)
}

# rounding for integers and tolerance for doubles; helpers preserve dimensions
numeric_differs <- \(x, y, digits, col) {
  abbr <- vec_ptype_abbr(strip_dims(x))
  if (abbr != "dbl") {
    if (is.null(digits)) return(neq(x, y))
    if (abbr == "int64") {
      round_int64 <- \(v) try_fetch(
        round(v, digits),
        warning = \(cnd) abort(
          glue("rounding to {digits} digits is too coarse for int64 col {col}"),
          parent = cnd
        )
      )
      return(neq(round_int64(x), round_int64(y)))
    }
    return(neq(x |> round(digits), y |> round(digits)))
  }
  if (is.null(digits)) return(neq(x, y))
  tol <- 10^-digits / 2
  ex <- neq(x, y)  # exact equality, treating NaN as NA

  # warn when a differing value's resolution is coarser than the tolerance
  vals <- c(x[ex], y[ex]) |> keep(is.finite)
  v <- vals[vals |> abs() |> which.max()]
  if (length(v) > 0 && numeric_ulp(v) > tol) {
    warn(glue(
      "column {col} has a value {f_num(v)}; floating point representation is less ",
      "accurate than requested tolerance {format(tol, scientific = TRUE)}"))
  }

  rg <- abs(x - y)  # raw gap at the tolerance scale
  ex & (is.na(rg) | rg > tol)  # inclusive at half a unit
}
