
context("Test S3 Group Generics for Ops")

syms_init(x, y, z, a, b, c, real = TRUE, .quite = TRUE)

test_that("Ops operators", {
    expect_true(R(x >= x - 1))

    expect_true(R(x / 2L == x * 0.5))
})
