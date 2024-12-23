
<!-- README.md is generated from README.Rmd. Please edit that file -->

# qlcheckr

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/qlcheckr)](https://CRAN.R-project.org/package=qlcheckr)
[![R-CMD-check](https://github.com/learnr-academy/qlcheckr/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/learnr-academy/qlcheckr/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

`qlcheckr` is an R package designed to analyse parsed user code by
searching for specific patterns within its Abstract Syntax Tree (AST).
The package is ideal for interactive code evaluation in Quarto Live
environments, enabling precise checks for function usage, arguments, and
expressions.

## Features

- **Function Usage Detection**: Verify if a specific function is used in
  the code.
- **Argument Matching**: Identify the presence of specific arguments in
  function calls.
- **Expression Searching**: Detect custom expressions within the code.
- **Flexible Pattern Matching**: Combine searches for functions,
  arguments, and expressions.

## Installation

To install the development version of `qlcheckr` from GitHub, use the
`remotes` package:

``` r
# Install remotes if not already installed
install.packages("remotes")

# Install qlcheckr
remotes::install_github("learnr-academy/qlcheckr")
```

## Usage

### Basic Examples

The `search_ast` function allows you to search for specific patterns in
parsed R code.

#### Search for a Function Name

``` r
library(qlcheckr)
#> 
#> Attaching package: 'qlcheckr'
#> The following object is masked from 'package:base':
#> 
#>     warnings

code <- quote(mean(x))
search_ast(code, .fn = "mean")
#> [1] FALSE
```

#### Search for an Argument

``` r
code <- quote(mean(x, na.rm = TRUE))
search_ast(code, na.rm = TRUE)
#> [1] TRUE
search_ast(code, na.rm = FALSE)
#> [1] FALSE
```

#### Search for an Expression

``` r
code <- quote(mean(x + y))
search_ast(code, .expr = x + y)
#> [1] TRUE
```

### Using `exists_in`

The `exists_in()` function checks whether elements in a list satisfy
specific conditions. It is particularly useful when working with
multiple outputs to pinpoint a result or identify errors.

The `.evaluate_result` object available in the check chunk of
quarto-live exercises contains information about results, errors,
warnings, messages and more. A manually constructed `.evaluate_result`
is shown here for testing purposes:

``` r
.evaluate_result <- evaluate::evaluate(
'print(rnorm(10))
sample$x
log(-1)
message("Hello world")',
output_handler = ql_output_handler)
```

<!-- TODO: Create dummy `.evaluate_results` and directly use `results()`, `errors()`, ... -->

#### Find a Matching Result

Suppose you are processing a list of numeric outputs and want to find if
any value is greater than a threshold:

``` r
results()
#> [[1]]
#>  [1] -0.6264538  0.1836433 -0.8356286  1.5952808  0.3295078 -0.8204684
#>  [7]  0.4874291  0.7383247  0.5757814 -0.3053884
#> 
#> [[2]]
#> [1] NaN
#> 
#> [[3]]
#> NULL

exists_in(results(), ~ any(. > 0.5))
#> [1] TRUE
```

This helps quickly confirm if any result (among many outputs) passes
your test.

#### Detecting specific errors

Consider checking code that might generate errors, which can be obtained
in Quarto Live check environments with the `errors()` helper.
`exists_in()` can again be used to search for specific errors.

``` r
errors()
#> [1] "object of type 'closure' is not subsettable"

exists_in(errors(), grepl, pattern = "not subsettable")
#> [1] TRUE
exists_in(errors(), grepl, pattern = "non-numeric argument")
#> [1] FALSE
```

This is valuable for identifying and debugging specific errors among
many possible results.

It is also possible to search warnings and messages with similar
helpers:

``` r
warnings()
#> [1] "NaNs produced"
messages()
#> [1] "Hello world\n"
```

## Contributing

We welcome contributions to `qlcheckr`! If you encounter any issues or
have suggestions for improvement, please open an issue or submit a pull
request on [GitHub](https://github.com/learnr-academy/qlcheckr).

## License

`qlcheckr` is licensed under the MIT License. See the [LICENSE](LICENSE)
file for details.
