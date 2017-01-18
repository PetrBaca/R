# linear regression test

x <- rnorm(10)
y <- 2 * x + rnorm(10)

lm <- lm(y ~ x)

plot(x, y)
abline(lm, col = 2)


