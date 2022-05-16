test_that("age_to_class works", {
  # setup
  ages <- c(18, 19, 12, 13, 6, 7, 0, 1, 0, 0, 0, 0)
  days <- c(NA, 6972, 4607, 4972, 2504, 2835, 279, 644, 31, 30, 0, 400)


  # evaluation
  age_classes <- age_to_class(ages, days)
  res <- ordered(
    c(
      "adolescente", "adulto", "eta scolare", "adolescente",
      "eta prescolare", "eta scolare", "lattante", "eta prescolare",
      "lattante", "neonato", "[wrong/missing age]",
      "[wrong/missing age]"
    ),
    levels = c(
      "neonato", "lattante", "eta prescolare", "eta scolare",
      "adolescente", "adulto", "[wrong/missing age]"
    )
  )

  # test
  expect_equal(age_classes, res)

})


test_that("factorize_centers works", {
  # setup
  centers_ids <- c(1, 3, 4)

  # evaluation
  centers_factorized_nocity <- factorize_centers(centers_ids, FALSE)
  centers_factorized_city <- factorize_centers(centers_ids)


  no_city_res <- factor(
    c(1, 3, 4),
    levels = centers_table[["id"]],
    labels = centers_table[["center"]]
  )
  city_res <- factor(
    c(1, 3, 4),
    levels = centers_table[["id"]],
    labels = stringr::str_c(
      centers_table[["center"]],
      " (", centers_table[["center_city"]], ")"
    ) |>
      stringr::str_trim()
  )

  # test
  expect_equal(centers_factorized_nocity, no_city_res)
  expect_equal(centers_factorized_city, city_res)

})
