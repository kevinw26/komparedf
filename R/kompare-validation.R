# shared input validation
# reject non-frames, empty frames, and frames with no columns
check_frame <- \(x, name) {
  if (!is.data.frame(x)) {
    abort(glue("{name} must be a data frame, not {class(x)[1]}"))
  }
  nms <- names(x)
  if (nrow(x) == 0) {
    abort(glue("{name} must not be empty"))
  }
  if (ncol(x) == 0) {
    abort(glue("{name} must have at least one column"))
  }
  if (anyNA(nms)) {
    abort(glue("{name} has NA column names"))
  }
  if (any(nms == "")) {
    abort(glue("{name} has empty column names"))
  }
  if (any(str_to_lower(nms) == ".rn")) {
    abort(glue("{name} has a reserved column name: .rn"))
  }
  if (nms |> str_detect("\x1f") |> any()) {
    abort(glue("{name} has a column name containing the unit separator (\\x1f)"))
  }
  invisible(x)
}

# validate a single integer within bounds; optionally nullable and capped
check_int <- \(
  x, name = "i", min = 1, max = .Machine$integer.max,
  allow_null = FALSE, cap = FALSE
) {
  bound <- case_when(
    min == 0 && max == .Machine$integer.max ~ "non-negative integer",
    min == 1 && max == .Machine$integer.max ~ "positive integer",
    TRUE ~ glue("integer between {min} and {max}")
  )
  prefix <- if (allow_null) "NULL or " else ""
  invalid <- \() abort(glue("{name} must be {prefix}a single {bound} not {x}"))

  if (is.null(x)) {
    if (allow_null) return(NULL)
    invalid()
  }
  if (!is_scalar_integerish(x) || is.na(x)) invalid()
  if (x < min) invalid()
  if (x > max) {
    if (cap) return(as.integer(max))
    invalid()
  }
  as.integer(x)
}

# short type label, with factor levels so order differences are visible
type_abbr <- \(x) {
  if (is.factor(x)) {
    kind <- if (is.ordered(x)) "ord" else "fct"
    return(glue("{kind}[{str_c(levels(x), collapse = ',')}]"))
  }
  vec_ptype_abbr(x)
}

# atomic columns on both sides
is_atomic_pair <- \(l, r, by_l, by_r = by_l) {
  map2_lgl(by_l, by_r, \(x, y) is.atomic(l[[x]]) && is.atomic(r[[y]]))
}

# reject recursive keys and keys without a common type
check_by_types <- \(
  l, r, by_l, by_r = by_l,
  origin = "by columns must have compatible types in l and r"
) {
  if (is.null(by_l)) return(invisible(by_l))
  non_atomic <- !is_atomic_pair(l, r, by_l, by_r)
  if (any(non_atomic)) {
    abort(glue(
      "by columns must be atomic, not list or other recursive types: ",
      "{str_c(by_l[non_atomic], collapse = ', ')}"
    ))
  }
  okay <- map2_lgl(by_l, by_r, \(x, y) has_common_dtype(l[[x]], r[[y]]))
  if (!all(okay)) {
    bad <- which(!okay) |>
      map_chr(\(i) {
        glue("{by_l[i]} ({type_abbr(l[[by_l[i]]])} vs {type_abbr(r[[by_r[i]]])})")
      })
    abort(glue("{origin}: {str_c(bad, collapse = ', ')}"))
  }
  invisible(by_l)
}

# validate matching by columns against both frames
check_by <- \(
  l, r, by_l, by_r = by_l, check_types = TRUE,
  type_origin = "by columns must have compatible types in l and r"
) {
  check_by_values(c(by_l, by_r))

  reserved <- union(by_l, by_r) |> intersect(c("col", ".l", ".r"))
  if (length(reserved) > 0) {
    abort(glue("by cannot use reserved column names: {str_c(reserved, collapse = ', ')}"))
  }

  for (side in c("l", "r")) {
    side_by <- if (side == "l") by_l else by_r
    repeats <- side_by[duplicated(side_by)] |>
      unique() |>
      str_c(collapse = ", ")
    if (nzchar(repeats)) {
      abort(glue("by repeats a column on the {side} side: {repeats}"))
    }
  }

  check_by_exists(by_l, names(l), "l")
  check_by_exists(by_r, names(r), "r")
  if (check_types) check_by_types(l, r, by_l, by_r, origin = type_origin)
  invisible(NULL)
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
