test_that("multiplication works", {
  to_factorize <- tibble(
    foo = c("a", "a", "b"),
    bar = 1:3,
    baz = c("one", "two", "three"),
    qux = c("random", "sample", "text")
  )

  fct_levels <- list(
    foo = c("a", "b", "c", "d"),
    baz = c("one", "two", "three")
  )

  factorized <- factorize(to_factorize, fct_levels)

  expect_is(factorized, class(to_factorize))
  expect_is(factorized[["foo"]], "factor")
  expect_equal(
    levels(factorized[["foo"]]), fct_levels[[1]]
  )
})
