test_that("plot checkbox works", {
  # setup
  test_db <- tibble::tribble(
    ~centro, ~a, ~b, ~c,
    "A",    "a", NA, "c",
    "A",    NA, NA, "c",
    "A",    NA, NA, NA,
    "A",    NA, "b", NA,
    "B",    NA, NA, "c",
    "B",    NA, "b", "c",
    "B",    NA, "b", "c"
  )
})

