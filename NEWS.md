# News

## 0.1.0
- Initial release: reports schema, key, and value differences between two data frames
- Flexible row matching using `dplyr::join_by()`, character vectors, or positional comparison
- Case-insensitive column matching and numeric tolerance control via `digits`
- Customizable string comparison with `ignorecase` and `trim` options
- Smart handling of factors and automatic conversion of midnight `POSIXct` to `Date`
- Integrated results with `tidy()` output, `summary()` reports, and `raise_error` support
