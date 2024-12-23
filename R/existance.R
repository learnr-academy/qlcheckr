#' Check for Existence in a List Based on a Condition
#'
#' Evaluates whether elements in a list satisfy a given condition and combines
#' the results using a specified logical function (e.g., `any` or `all`).
#'
#' @param .x A list or vector to be evaluated.
#' @param .f A predicate function or a formula. If a formula is provided, it
#'   will be converted to a function. The formula should use `.` to refer to
#'   elements of `.x`.
#' @param ... Additional arguments passed to `.f`.
#' @param .require A logical function to combine the results of applying `.f`
#'   to each element of `.x`. Default is [any()], which returns `TRUE` if any
#'   elements satisfy the condition.
#'
#' @return A logical value. `TRUE` if `.require` evaluates to `TRUE` for the
#'   results of applying `.f` to `.x`, otherwise `FALSE`.
#'
#' @examples
#' # Example with a function
#' exists_in(list(1, 2, 3), function(x) x > 2)
#' # Returns TRUE (since 3 > 2)
#'
#' # Example with a formula
#' exists_in(list(1, 2, 3), ~ . > 2)
#' # Returns TRUE (since 3 > 2)
#'
#' # Example requiring all elements to satisfy the condition
#' exists_in(list(1, 2, 3), ~ . > 0, .require = all)
#' # Returns TRUE (all elements are > 0)
#'
#' # Example with additional arguments
#' exists_in(list("apple", "banana", "cherry"), grepl, pattern = "a")
#' # Returns TRUE (some elements contain "a")
#'
#' @export
exists_in <- function(.x, .f, ..., .require = any) {
  if(inherits(.f, "formula")) {
    f <- .f
    .f <- function(.x, .y, ..., . = .x) {}
    body(.f) <- f[[length(f)]]
  }
  .require(vapply(.x, function(...) isTRUE(.f(...)), logical(1L), ...))
}
