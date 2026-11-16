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
- prefer pipes `x |> g() |> f()` to function composition `f(g(x))` but not when multiple inputs
  are similar in length or using pipes would require inserting parentheses

## messages

- error, warning, and inform messages start lower case
- join clauses with semicolons
- no terminal full stop

## comments

- lowercase, terse, sentence fragments, no full stops
- explain why, not what

## scope

- change only what was asked
- don't refactor, reformat, or fix unrelated code in passing
- flag other issues in the reply, don't fix them
- don't add dependencies, files, or config without asking
- however when there is an often used base function in tidyverse that does what you are looking to
  do, feel free to add it
- try to minimise noise in the git diff
