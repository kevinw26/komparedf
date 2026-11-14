# agents

## writing

- british english spelling
- brief. no preamble, no restating the request, no praise
- don't explain unless asked or genuinely needed

## r style

- `\(x)` for anonymous functions, never `function(x)`
- snake_case for variables, args, and helpers
- tidyverse only: dplyr, tidyr, purrr, tibble, stringr, rlang
- rlang for tidy eval (`sym()`, `enquo()`, `!!!`, `set_names()`), not `get()`/`eval(parse())`
- stringr for all string work, not `gsub`/`substr`/`regexpr`

## comments

- lowercase, terse, sentence fragments, no full stops
- explain why, not what

## scope

- change only what was asked
- don't refactor, reformat, or fix unrelated code in passing
- flag other issues in the reply, don't fix them
- don't add dependencies, files, or config without asking
