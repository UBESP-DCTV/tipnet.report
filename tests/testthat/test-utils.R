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
