test_that("read_redcap can connect and correctly download data", {

  testthat::skip("10 minutes to run") # ~ 10 min

  # last success 2020-05-19T10:12:00Z
  expect_is(
    read_redcap(tipnet_redcap_url(), tipnet_token()),
    "list"
  )
})
