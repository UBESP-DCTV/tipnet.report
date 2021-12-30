sample_df <- tibble(
  fields = c("a", rep("r", 3)),
  sheet  = c("a", "b", "c", "a"),
  tables = list(
    tibble(f = "a", s = "a"),
    tibble(f = "r", s = "b"),
    tibble(f = "r", s = "c"),
    tibble(f = "r", s = "a")
  )
)

test_that("get_sheet works", {
  expect_is(get_sheet(sample_df, "b"), "tbl_df")
  expect_is(get_sheet(sample_df, "a", field = "r"), "tbl_df")
  expect_equal(
    get_sheet(sample_df, "a", field = "r"),
    tibble(f = "r", s = "a")
  )

  expect_error(
    get_sheet(sample_df, "e"),
    "does not exists",
    class = "usethis_error"
  )
  expect_error(
    get_sheet(sample_df, "a"),
    "is present in the",
    class = "usethis_error"
  )
  expect_error(
    get_sheet(sample_df, "b", field = "a"),
    "is not present in the field",
    class = "usethis_error"
  )
})
