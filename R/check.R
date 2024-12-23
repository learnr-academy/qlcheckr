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
#'   If `NULL`, expressions are not checked.
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

as_html_list <- function(x) {
  if (!length(x)) return(NULL)
  paste0(
    "<ul>",
    sprintf("  <li>%s</li>\n", format(x)),
    "</ul>"
  )
}

#' Apply Code Checks for Quarto Live with Feedback and Debugging
#'
#' This function evaluates user code against a series of named tests, providing
#' feedback based on the test outcomes. It integrates with Quarto Live to
#' facilitate interactive code checking.
#'
#' @param ... Named logical expressions representing the tests to be evaluated.
#'   Each test's name should provide a hint or message to display when the test fails.
#'   A test passes if it evaluates to `FALSE` and fails if it evaluates to `TRUE`.
#' @param .msg_correct Character. The message to display when all tests pass.
#'   Default is `"That's correct! Well done."`.
#' @param .msg_incorrect Character. The message to display when any test fails.
#'   Default is `"That's incorrect! Please try again..."`.
#' @param .debug Logical. If `TRUE`, prints detailed debug information, including
#'   the state of all tests and additional internal evaluation details. Default is `FALSE`.
#'
#' @return Returns `.msg_correct` if all tests pass (`FALSE`), or `.msg_incorrect`
#'   followed by the failure hints for tests that failed (`TRUE`).
#'   When `.debug` is `TRUE`, additional debug information is printed.
#'
#' @examples
#' # Example usage:
#' apply_checks(
#'   "Your math doesn't work correctly." = 1 + 1 != 2,
#'   "Your logic is flawed." = 3 * 3 != 9
#' )
#' # Returns: "That's correct! Well done."
#'
#' apply_checks(
#'   "Your math doesn't work correctly." = 1 + 1 != 2,
#'   "Your logic is flawed." = 3 * 3 != 8
#' )
#' # Returns:
#' # "That's incorrect! Please try again...
#' # - Your logic is flawed."
#'
#' apply_checks(
#'   "Your math doesn't work correctly." = 1 + 1 != 2,
#'   .debug = TRUE
#' )
#' # Debug output includes detailed evaluation results and failure messages.
#'
#' @export
apply_checks <- function(
    ...,
    .msg_correct = "That's correct! Well done.",  #"✅ That's correct! Well done 🎉",
    .msg_incorrect = "That's incorrect! Please try again...", #"❌ That's incorrect! Please try again...",
    .debug = FALSE
) {
  checks <- as.logical(c(...))

  res <- list(correct = all(!checks))

  res$message <- paste(
    c(
      "<b>",
      if (res$correct) .msg_correct else .msg_incorrect,
      "</b>",
      as_html_list(names(checks)[checks])
    ),
    collapse = "<br>"
  )

  res$type <- if (res$correct) "success" else "warning"

  if (.debug) {
    check_opts <- c(".check_code", ".engine", ".envir_prep", ".envir_result",
                    ".evaluate_result", ".label", ".last_value", ".result",
                    ".solution_code", ".solution",
                    ".stage", ".user_code")
    check_env <- as.list(parent.frame(3L))
    check_loc <- match(substring(check_opts, 2), names(check_env))
    check_env <- check_env[na.omit(check_loc)]
    names(check_env) <- check_opts[!is.na(check_loc)]
    res$message <- paste(
      c(
        res$message,
        "<br><b>Debug information</b>",
        capture.output(str(check_env)),
        capture.output(str(list(
          `results()` = results(),
          `errors()` = errors(),
          `warnings()` = warnings(),
          `messages()` = messages()
        )))
      ),
      collapse = "<br>"
    )
  }

  res
}
