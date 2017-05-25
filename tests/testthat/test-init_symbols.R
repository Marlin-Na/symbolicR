context("Test Methods for Symbol Initiation")

test_that("Test `syms_init`", {
    syms_init(x, .quite = TRUE, positive = TRUE, y)
    expect_true(exists("x"))
    expect_true(exists("y"))
    expect_true(R(x >= 0))
    expect_true(R(y >= 0))
    expect_null(environment(x))
    expect_null(environment(y))
})
