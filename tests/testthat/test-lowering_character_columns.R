test_that("character columns will be lowered but not removed", {
    source <- tibble(one = c("A", "b"), two = c("d", "E"), three = 1:2)
    target <- tibble(one = c("a", "b"), two = c("d", "e"), three = 1:2)
    expect_equal(lowering_character_columns(source), target)
})
