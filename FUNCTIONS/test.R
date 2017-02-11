# linear regression test
x <- rnorm(10)
y <- 2 * x + rnorm(10)
y

linmod <- lm(y ~ x - 1)
summary(linmod)

plot(x, y)
abline(linmod, col = 2)

summary(linmod)
