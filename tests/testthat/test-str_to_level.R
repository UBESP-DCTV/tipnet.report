test_that("str_to_level works", {
  coded_simple <- "1, maschio | 2, femmina | 3, ambiguo"
  coded_spaced <- paste(
    "1, caucasica",
    "2, ispanica",
    "3, asiatica",
    "4, africana",
    "5, araba",
    "6, etnia mista",
    sep = " | "
  )
  coded_na <- NA_character_
  coded_duplicate <- "1, a | 2, b | 3, a"

  expected_simple <- c("maschio", "femmina", "ambiguo")
  expected_spaced <- c(
    "caucasica", "ispanica", "asiatica", "africana", "araba",
    "etnia mista"
  )
  expected_na <- character()
  expected_duplicate <- c("a", "b")

  expect_equal(str_to_level(coded_simple), expected_simple)
  expect_equal(str_to_level(coded_spaced), expected_spaced)
  expect_equal(str_to_level(coded_na), expected_na)
  expect_equal(str_to_level(coded_duplicate), expected_duplicate)
})


test_that("str_to_level manage wrong input", {

  expect_error(str_to_level(""), "All elements must have > 0 chars")
  expect_error(
    str_to_level(c("1, a", "2, b")),
    "the condition has length > 1"
  )

})

test_that("str_to_level manage commas-levels", {
  coded_commas <- "1, a | 2, b, c | 3, d"
  expect_equal(str_to_level(coded_commas), c("a", "b, c", "d"))

})
