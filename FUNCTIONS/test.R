
par(mfrow = c(4, 4), cex = 0.5, mar = c (2,2,2,1))
for (i in 1:16) {
x <- rnorm(1000)
hist(x, main = paste0("Histogram of x ", i))
}

