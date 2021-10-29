test_that("integer column are converted and not collapsed", {
    source <- tibble(
        a = c(1, 2),
        aa = c(1, NA),
        b = c(1L, 2),
        c = c(1, 2L),
        d = c(1L, 2L),
        e = c(1.2, 1L),
        f = c("a", "b")
    )

    target <- tibble(
        a = c(1L, 2L),
        aa = c(1L, NA),
        b = c(1L, 2L),
        c = c(1L, 2L),
        d = c(1L, 2L),
        e = c(1.2, 1L),
        f = c("a", "b")
    )
    expect_identical(
        convert_to_integer_numerical_integer_columns(source),
        target
    )
})


test_that("behave correctly in strange occasion", {
    # see https://bit.ly/2NTzW92

 expect_trues <- tibble(
    cl  = sqrt(2)^2,
    pp  = 9.0,
    t   = 1 / (1 - 0.98),
    ar0 = 66L,
    ar1 = 66,
    ar2 = 1 + 2^-50,
    v   = 222e3,
    w1  = 1e4,
    w2  = 1e5,
    # v2  = "1000000000000000000000000000000000001", # too big
    an   = 2 / 49 * 49,
    # ju1 = 1e22, # too big
    # ju2 = 1e24, # too big
    al  = floor(1),
    v5  = 1.0000000000000001 # this is under machine precision!
)

target_trues <- tibble(
    cl  = 2L,
    pp  = 9L,
    t   = 50L,
    ar0 = 66L,
    ar1 = 66L,
    ar2 = 1L,
    v   = 222e3L,
    w1  = 1e4L,
    w2  = 1e5L,
    # v2  = 1000000000000000000000000000000000001L, # too big
    an   = 2L,
    # ju1 = 1e22, # too big
    # ju2 = 1e24, # too big
    al  = 1L,
    v5  = 1L
)

expect_falses <- tibble(
    bb  = 5 - 1e-8,
    pt1 = 1.0000001,
    pt2 = 1.00000001,
    v3  = 3243.34,
    v4  = "sdfds"
)

expect_identical(
    convert_to_integer_numerical_integer_columns(expect_trues),
    target_trues
)

expect_identical(
    convert_to_integer_numerical_integer_columns(expect_falses),
    expect_falses
)

})


test_that("convert_to_integer works", {
    expect_trues <- c(
        cl  = sqrt(2)^2,
        pp  = 9.0,
        t   = 1 / (1 - 0.98),
        ar0 = 66L,
        ar1 = 66,
        ar2 = 1 + 2^-50,
        v   = 222e3,
        w1  = 1e4,
        w2  = 1e5,
        an   = 2 / 49*49,
        al  = floor(1),
        v5  = 1.0000000000000001 # this is under machine precision!
    )

    target_trues <- c(
        cl  = 2L,
        pp  = 9L,
        t   = 50L,
        ar0 = 66L,
        ar1 = 66L,
        ar2 = 1L,
        v   = 222e3L,
        w1  = 1e4L,
        w2  = 1e5L,
        an   = 2L,
        al  = 1L,
        v5  = 1L
    )

    expect_identical(convert_to_integer(expect_trues), target_trues)
})
