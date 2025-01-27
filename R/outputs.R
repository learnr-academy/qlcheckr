# Search the parent frames for the evaluation results
# Return NULL if they could not be found
get_evaluate_result <- function(i = 1) {
  found <- FALSE
  while (!found && !identical(parent.frame(i), .GlobalEnv)) {
    found <- exists(".evaluate_result", env = parent.frame(i))
    i <- i + 1
  }
  if (found) get(".evaluate_result", parent.frame(i-1)) else NULL
}

outputs_factory <- function(class, field, vec = FALSE) {
  force(class)
  force(field)
  force(vec)
  function() {
    outputs <- get_evaluate_result()
    res <- Filter(\(x) inherits(x, class), outputs)
    res <- lapply(res, `[[`, field)
    if(vec) {
      unlist(res, recursive = FALSE)
    } else {
      res
    }
  }
}

#' Extract Common Output Types from an Evaluate Object
#'
#' These functions facilitate the extraction of specific output types, such as results, errors, warnings,
#' or messages, from an `evaluate` object. The outputs are filtered based on their class and a specified
#' field, making it easier to analyse the results of code evaluation in Quarto Live exercise grading.
#'
#' @details
#' The functions operate on an `.evaluate_result` object, which stores outputs from code execution
#' (e.g., in Quarto Live exercises). They are designed to extract specific elements such as:
#' - **Results**: Extracts the `value` field from objects of class `"results"`.
#' - **Errors**: Extracts the `message` field from objects of class `"error"`.
#' - **Warnings**: Extracts the `message` field from objects of class `"warning"`.
#' - **Messages**: Extracts the `message` field from objects of class `"message"`.
#'
#' @param class Character. The class of objects to extract from the `evaluate` output.
#' @param field Character. The field within the object to extract (e.g., `"value"` or `"message"`).
#'
#' @return A list containing the extracted fields from the specified class of outputs.
#'
#' @examplesIf requireNamespace("evaluate", quietly = TRUE)
#'
#' .evaluate_result <- evaluate::evaluate(
#' 'print(rnorm(10))
#' sample$x
#' log(-1)
#' message("Hello world")', output_handler = ql_output_handler)
#'
#' # Extract results
#' ql_results()
#'
#' # Extract errors
#' ql_errors()
#'
#' # Extract warnings
#' ql_warnings()
#'
#' # Extract messages
#' ql_messages()
#'
#' @name outputs-extraction
#' @aliases outputs results errors warnings messages
#' @export
ql_outputs <- function(class, field) outputs_factory(class, field)()

#' @rdname outputs-extraction
#' @export
ql_results <- outputs_factory("result", "value")

#' @rdname outputs-extraction
#' @export
ql_errors <- outputs_factory("error", "message", vec = TRUE)

#' @rdname outputs-extraction
#' @export
ql_warnings <- outputs_factory("warning", "message", vec = TRUE)

#' @rdname outputs-extraction
#' @export
ql_messages <- outputs_factory("message", "message", vec = TRUE)

#' @rdname outputs-extraction
#' @export
ql_src <- outputs_factory("source", "src", vec = TRUE)

#' @rdname outputs-extraction
#' @export
ql_ast <- function() {
  parse(text = ql_src())
}

#' This mimics the behaviour of quarto-live evaluate results
#'
#' @keywords internal
#' @export
#'
# dput(evaluate::new_output_handler(
#   value = function(x, visible) {
#     res <- list(value = x, visible = FALSE)
#     res$class <- class(res$value)
#     class(res) <- "result"
#     res
#   }
# ))
ql_output_handler <-structure(
  list(
    source = identity,
    text = identity,
    graphics = identity,
    message = identity,
    warning = identity,
    error = identity,
    value = function (x, visible) {
      res <- list(value = x, visible = FALSE)
      res$class <- class(res$value)
      class(res) <- "result"
      res
    },
    calling_handlers = list()
  ),
  class = "output_handler"
)

