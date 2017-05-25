context("Test Group Generics for Symbolic Expressions")


test_that("Ops (operators) Group Methods", {
    syms_init(x, y, z, a, b, c, real = TRUE, .quite = TRUE)

    expect_true(R(x >= x - 1))

    expect_true(R(x / 2L == x * 0.5))
})


test_that("Math Group Methods", {

})


test_that("Argument matching with Math group methods", {
    syms_init(x, y, z, .quite = TRUE)

    expect_error(log(x, bsa = 5))
                 log(x, bas = 5)
    expect_error(sin(y, 12))
})

