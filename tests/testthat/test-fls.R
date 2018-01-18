context("Testing `fls` function")

X = matrix(c(1,2,3,1.01,0.99,1), ncol = 2)
y = c(2.01,2.99,4)
test.fls = fls(X, y, mu = 1)

test_that("`fls` coef", {
  expect_identical(all(abs(coef(test.fls) - 1) < 1e015), TRUE)
})

test_that("`fls` errors", {
  expect_equal(test.fls$r_D, 0)
  expect_equal(test.fls$r_M, 0)
})

