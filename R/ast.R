compare_args <- function(x, y) {
  val <- match(x, y)
  if(any(is.na(val))) return(FALSE)
  identical(names(x), names(y)[val])
}

search_ast_impl <- function(.code, fn = NULL, fn_args = list()) {
  if(!(is.call(.code) || is.expression(.code))) return(FALSE)
  args <- as.list(.code[-1])
  if(compare_args(fn_args, args)) {
    if(is.null(fn) || identical(fn, .code[[1]])) return(TRUE)
  }

  any(vapply(args, search_ast_impl, logical(1L), fn = fn, fn_args = fn_args))
}

#' Search Abstract Syntax Tree (AST) for Specific Patterns
#'
#' Analyses parsed user code to identify the presence of specific patterns in the Abstract Syntax Tree (AST),
#' such as the usage of functions, arguments, or expressions.
#'
#' @param .code A parsed R expression or a call object representing the user code to analyse.
#'   Typically obtained using [parse()] or similar methods.
#' @param .fn Character or NULL. The name of the function to search for in the code.
#'   If `NULL`, the function usage is not checked.
#' @param ... Additional arguments to search for in function calls.
#'   These may include named or unnamed arguments.
#' @param .expr A quoted or parsed R expression to search for within the code.
#'   If `NULL`, expressions are not checked. Using `.expr` has priority over
#'   `.fn` and `...`, if both conditions are specified only `.expr` will be
#'   used.
#'
#' @return A logical value indicating whether the specified patterns were found
#'   in the provided code (`TRUE`) or not (`FALSE`).
#'
#' @details
#' The function operates on the AST representation of the provided code, enabling
#' precise pattern matching. Use `.fn` to check for the presence of a specific
#' function, `...` to search for arguments, and `.expr` for custom expression
#' matching. Any combination of these can be used to tailor the search criteria.
#'
#' @examples
#' # Example: Search for a specific function name
#' search_ast(quote(mean(x)), .fn = mean)
#'
#' # Example: Search for a specific argument
#' search_ast(quote(mean(x, na.rm = TRUE)), na.rm = TRUE)
#' search_ast(quote(mean(x, na.rm = TRUE)), na.rm = FALSE)
#'
#' # Example: Search for an expression
#' search_ast(quote(mean(x + y)), .expr = x + y)
#' search_ast(quote(mean(x + y)), .expr = mean(x + y))
#' search_ast(quote(mean(x + y)), .expr = log(x + y))
#'
#' @export
search_ast <- function(.code, .fn = NULL, ..., .expr = NULL) {
  .expr <- substitute(.expr)
  if(!is.null(.expr)) {
    .fn <- .expr[[1]]
    fn_args <- as.list(.expr[-1])
  } else {
    .fn <- substitute(.fn)
    fn_args <- as.list(substitute(...()))
  }

  if (is.language(.code)) .code <- as.expression(.code)
  if (is.expression(.code)) .code <- as.list(.code)
  any(vapply(.code, search_ast_impl, logical(1L), fn = .fn, fn_args = fn_args))
}
