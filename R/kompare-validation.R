# shared input validation
# reject non-frames, empty frames, and frames with no columns
check_frame <- \(x, name) {
  if (!is.data.frame(x)) {
    abort(glue("{name} must be a data frame, not {class(x)[1]}"))
  }
  if (nrow(x) == 0) {
    abort(glue("{name} must not be empty"))
  }
  if (ncol(x) == 0) {
    abort(glue("{name} must have at least one column"))
  }
  if (anyNA(names(x))) {
    abort(glue("{name} has NA column names"))
  }
  if (any(names(x) == "")) {
    abort(glue("{name} has empty column names"))
  }
  if (any(str_to_lower(names(x)) == ".rn")) {
    abort(glue("{name} has a reserved column name: .rn"))
  }
  if (any(str_detect(names(x), "\x1f"))) {
    abort(glue("{name} has a column name containing the unit separator (\\x1f)"))
  }
  invisible(x)
}

# reject negative, fractional, or non-finite counts
check_count <- \(x, name) {
  if (!is_scalar_integerish(x) || !is.finite(x) || x < 0) {
    abort(glue("{name} must be a single non-negative integer"))
  }
  as.integer(x)
}

# reject repeated by columns on either side
check_by_repeats <- \(lhs, rhs) {
  check_side <- \(x, side) {
    if (anyDuplicated(x) == 0) return(invisible(x))
    repeats <- x[duplicated(x)] |>
      unique() |>
      str_c(collapse = ", ")
    abort(glue("by repeats a column on the {side} side: {repeats}"))
  }
  check_side(lhs, "l")
  check_side(rhs, "r")
  invisible(NULL)
}

# reject recursive keys and keys without a common type
check_by_types <- \(l, r, by, origin = "by columns must have compatible types in l and r") {
  non_atomic <- discard(by, \(col) is.atomic(l[[col]]) && is.atomic(r[[col]]))
  if (length(non_atomic) > 0) {
    abort(glue(
      "by columns must be atomic, not list or other recursive types: ",
      "{str_c(non_atomic, collapse = ', ')}"
    ))
  }
  ok <- map_lgl(by, \(col) {
    try_fetch(
      {
        vec_ptype_common(l[[col]], r[[col]])
        TRUE
      },
      error = \(cnd) FALSE
    )
  })
  if (!all(ok)) {
    abort(glue("{origin}: {str_c(by[!ok], collapse = ', ')}"))
  }
  invisible(by)
}

# reject by columns missing from a side
check_by_exists <- \(by, cols, side) {
  missing <- setdiff(by, cols)
  if (length(missing) > 0) {
    abort(glue("by columns missing from {side}: {str_c(missing, collapse = ', ')}"))
  }
  invisible(by)
}

# reject missing or empty by names
check_by_values <- \(by) {
  if (anyNA(by) || any(by == "")) {
    abort("by must not be NA or empty")
  }
  invisible(by)
}

# representation guard for a numeric pair: 1.5 ulp of the coarser value,
# covering half an ulp per operand and half from the subtraction
ulp_guard <- \(x, y) {
  to_ulp <- \(v) {
    v <- abs(as.numeric(v))
    pmax(2^(floor(log2(v)) - 52), .Machine$double.xmin * .Machine$double.eps)
  }
  guard <- 1.5 * pmax(to_ulp(x), to_ulp(y))
  if_else(is.finite(guard), guard, 0) # non-finite values carry no finite guard
}
