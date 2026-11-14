# komparedf

Compare two data frames and report schema, key, and value differences.

## Install

```r
# install.packages("devtools")
devtools::install_github("kevinw26/komparedf")
```

## Use

```r
library(df.kompare)
library(dplyr)

l <- tibble(id = 1:3, price = c(10, 20, 30))
r <- tibble(id = 1:4, price = c(10, 21, 30, 40))

kompare(l, r, by = join_by(id))
```
```
# kompare: 3 x 2 vs 4 x 2, by: id
schema mismatches: 0
unmatched keys: 1 (0 in l only, 1 in r only)
       id .l    .r   
  1     4 NA    TRUE 

value differences by type:
    type     id col   .l    .r   
  1 dbl       2 price 20    21   

compared 3 rows and 2 columns

ℹ 2 differences found
ℹ l and r have different lengths (3 vs 4)
```

`by` takes a `join_by()` object, a character vector of shared names, or a named
vector (`c(l_id = "r_id")`). Leave it `NULL` to compare rows by position.

## Results

```r
out <- kompare(l, r, by = "id")

out$schema   # columns on one side only, or retyped
out$keys     # rows with no match
out$diffs    # differing rows, by vctrs type ("dbl", "chr", ...)

is_equal(out)        # TRUE when nothing differs
as.logical(out)      # same
summary(out)         # print the report
format(out)          # report as lines
tidy(out)            # tibble: type, col, diffs (attach generics or broom)
```

## Options

| Argument      | Default       | Effect                                    |
|---------------|---------------|-------------------------------------------|
| `digits`      | `4`           | decimal places for numeric tolerance      |
| `strings`     | `"strict"`    | `"loose"` ignores case and outer spaces   |
| `factors`     | `"as_strings"`| `"as_factors"` unions levels first        |
| `ignore`      | `NULL`        | columns skipped in value comparisons      |
| `fn`          | `\(d) d`      | applied to both frames before comparing   |
| `raise_error` | `FALSE`       | abort with the report on differences      |

```r
kompare(l, r, by = "id", raise_error = TRUE)
#> Error: kompare found differences between l and r
```

Parameter names from `rCompare` – eg `keys`, `roundDigits`, and `round` – also work.

## Notes

- Column names are matched case-insensitively
- Only columns sharing a name and type are compared for values; anything else
  lands in `schema`
- `POSIXct` columns sitting at midnight are compared as `Date`, with a message
- `by` columns must be unique on each side
