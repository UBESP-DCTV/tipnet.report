test_that("fix_codpat does not change correct codes", {
  expect_equal(fix_codpat("1234-5"), "1234-5")
  expect_equal(fix_codpat("1234-56"), "1234-56")
  expect_equal(fix_codpat("123-4"), "123-4")
  expect_equal(fix_codpat("123-45"), "123-45")
})


test_that("fix_codpat correctly changes wrong codes", {
  expect_equal(fix_codpat("1234 - 5"), "1234-5")
  expect_equal(fix_codpat("1234.56"), "1234-56")
  expect_equal(fix_codpat("123/4"), "123-4")
  expect_equal(suppressWarnings(fix_codpat("123456")), "1234-56")
})

test_that("fix_codpat works vectorized", {
  expect_equal(
    fix_codpat(c("1234 - 5", "1234.56", "123/4", "123-4")),
    c("1234-5", "1234-56", "123-4", "123-4")
  )
})
