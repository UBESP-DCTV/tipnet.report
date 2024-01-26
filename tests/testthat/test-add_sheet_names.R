sample_df <- tibble(
    a = 1,
    b = 1,
    c = 1,
    c_complete = 1,
    d = 1,
    e_foo = 1,
    e_complete = 1,
    f_da_compilare_aaa_complete = 1
)

test_that("add_sheets_prefix works", {
  res <- add_sheets_prefix(sample_df)
  expected_names <- c(
    "c_a", "c_b", "c_c", "c_complete", "e_d", "e_e_foo", "e_complete",
    "f_complete"
  )

  expect_is(res, "tbl_df")
  expect_equal(names(res), expected_names)

})

test_that("add_sheets_prefix mange exeptions", {
  res <- add_sheets_prefix(sample_df, exept = "a")
  expect_equal(names(res)[[1]], "a")
})

rm(sample_df)
