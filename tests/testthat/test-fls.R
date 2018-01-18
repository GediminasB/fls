context("Testing `fls` function")

X = matrix(c(1,2,3,
             1.05,0.95,1.03), ncol = 2)
y = c(2.1,2.85,4.05)
test.fls = fls(X, y, mu = 1, smooth = TRUE)

Results = list(
  Coef = matrix(c(0.9293123, 0.9190147, 0.9639854, 1.105133, 1.094321, 1.109761), ncol = 2),
  r_D = 0.002483708,
  r_M = 0.001094396
)
test_that("`fls` coef", {
  expect_equal(coef(test.fls), Results$Coef, tolerance = 1e-6)
})

test_that("`fls` errors", {
  expect_equal(test.fls$r_D, Results$r_D, tolerance = 1e-9)
  expect_equal(test.fls$r_M, Results$r_M, tolerance = 1e-9)
})

