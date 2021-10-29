test_that("character no/si columns to FALSE/TRUE", {
    source <- tibble(
        one = c(
            "si", "SI", "Si", "sI", "sì", "Sì", "SÌ", "sÌ",
            "no", "NO", "No", "nO", NA
        ),
        two = letters[seq_along(one)],
        three = seq_along(one)
    )

    target <- tibble(
        one = c(rep(TRUE, 8), rep(FALSE, 4), NA),
        two = letters[seq_along(one)],
        three = seq_along(one)
    )
    expect_equal(nosi_columns_to_lgl(source), target)
})
