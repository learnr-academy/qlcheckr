# Test cases for search_ast()
test_that("search_ast() correctly identifies function usage", {
  code <- expression({
    x <- mean(1:10)
    y <- sum(x)
  })

  # Test for a specific function
  expect_true(search_ast(code, .fn = mean))
  expect_true(search_ast(code, .fn = sum))
  expect_false(search_ast(code, .fn = median))
})

test_that("search_ast() correctly identifies argument usage", {
  code <- expression({
    x <- mean(1:10, na.rm = TRUE)
  })

  # Test for argument usage
  expect_true(search_ast(code, na.rm = TRUE))
  expect_false(search_ast(code, trim = 0.1))
})

test_that("search_ast() correctly identifies expressions", {
  code <- expression({
    x <- mean(1:10)
    if (x > 5) print("High")
  })

  # Test for expression matching
  expect_true(search_ast(code, .expr = x > 5))
  expect_false(search_ast(code, .expr = x < 5))
})

test_that("search_ast() handles empty input gracefully", {
  code <- expression({})

  # Test with no code
  expect_false(search_ast(code, .fn = mean))
  expect_false(search_ast(code, .expr = expression(x > 5)))
})

test_that("search_ast() works with nested expressions", {
  code <- expression({
    x <- mean(1:10)
    y <- sum(x)
    z <- ifelse(y > 10, "High", "Low")
  })

  # Test for nested function and argument usage
  expect_true(search_ast(code, .fn = ifelse))
  expect_true(search_ast(code, .expr = y > 10))
})

