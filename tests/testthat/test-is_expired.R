test_that("is_expired works", {

  withr::with_file(
    list(
      "new" = writeLines("today", "new"),
      "old" = writeLines("a month ago", "old")
    ), {

      expect_false(is_expired("new", 28))

      Sys.setFileTime("old", lubridate::now() - lubridate::days(30))
      expect_true(is_expired("old", 28))
    }
  )
})
