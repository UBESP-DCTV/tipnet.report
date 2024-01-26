test_that("pipe works", {
  expect_equal(1 %>% sum(2), 3)
})
