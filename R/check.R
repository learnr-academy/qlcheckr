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
