# Test cases for exists_in()
test_that("exists_in() identifies the presence of a specific result", {
  outputs <- list(1, "hello", TRUE, 42)

  # Test for specific results
  expect_true(exists_in(outputs, .f = \(x) x == 42))
  expect_true(exists_in(outputs, .f = \(x) is.character(x)))
  expect_false(exists_in(outputs, .f = \(x) x == 100))

  # Test with a formula
  expect_true(exists_in(outputs, .f = ~ .x == 42))
  expect_false(exists_in(outputs, .f = ~ .x == 100))
})

test_that("exists_in() identifies the presence of errors", {
  outputs <- list(
    structure(list(message = "Error 1"), class = "error"),
    structure(list(message = "Error 2"), class = "error"),
    "normal output"
  )

  # Check for specific error messages
  expect_true(exists_in(outputs, .f = ~ inherits(.x, "error") && .x$message == "Error 1"))
  expect_false(exists_in(outputs, .f = ~ inherits(.x, "error") && .x$message == "Error 3"))
})

test_that("exists_in() works with custom predicates", {
  outputs <- list(1, 2, 3, 4, 5)

  # Test with a custom function
  expect_true(exists_in(outputs, .f = \(x) x %% 2 == 0))  # Finds an even number
  expect_false(exists_in(outputs, .f = \(x) x > 10))      # No values > 10
})

test_that("exists_in() respects the .require argument", {
  outputs <- list(TRUE, FALSE, TRUE, FALSE)

  # Test with .require = any (default)
  expect_true(exists_in(outputs, .f = ~ .x == TRUE, .require = any))

  # Test with .require = all
  expect_false(exists_in(outputs, .f = ~ .x == TRUE, .require = all))
  expect_true(exists_in(outputs, .f = ~ is.logical(.x), .require = all))
})

test_that("exists_in() handles empty input gracefully", {
  outputs <- list()

  # Test with empty input
  expect_false(exists_in(outputs, .f = ~ .x == 42))
})

test_that("exists_in() handles different types of output correctly", {
  outputs <- list(
    42,
    "string",
    structure(list(message = "An error"), class = "error"),
    list(sublist = 42)
  )

  # Test with mixed types
  expect_true(exists_in(outputs, .f = ~ is.numeric(.x) && .x == 42))
  expect_true(exists_in(outputs, .f = ~ is.list(.x) && "sublist" %in% names(.x)))
  expect_true(exists_in(outputs, .f = ~ inherits(.x, "error")))
})

test_that("exists_in() works with edge cases for .f", {
  outputs <- list(NULL, NA, Inf, NaN)

  # Test for NULL
  expect_true(exists_in(outputs, .f = ~ is.null(.x)))

  # Test for NA
  expect_true(exists_in(outputs, .f = ~ is.na(.x)))

  # Test for Inf
  expect_true(exists_in(outputs, .f = ~ is.infinite(.x)))

  # Test for NaN
  expect_true(exists_in(outputs, .f = ~ is.nan(.x)))
})
