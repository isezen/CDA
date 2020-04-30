library(ggplot2)
library(ggthemes)
library(MASS)
library(univOutl)

hbout <- function(x, r) {
  x <- x[is.finite(x)]
  stopifnot(length(x) > 0, all(x > 0))
  xref <- median(x)
  if (xref <= sqrt(.Machine$double.eps))
    warning("Reference value close to zero: results may be inaccurate")
  pmax(x/xref, xref/x) > r
}

# Generate gamma rvs
set.seed(1)
y <- rlnorm(1000, meanlog = 3.6772376, sdlog = 0.8197301)
den <- density(y)
dat <- data.frame(x = den$x, y = den$y)
ggplot(data = dat, aes(x = x, y = y)) +
  geom_line(size = 1) + theme_tufte(base_size = 16)

y1 <- y[1:999]
y2 <- y[2:1000]
x <- 1:1000
out <- HBmethod(y1, y2, C = c(20, 40))
plot(x, y, pch = 20)
points(x[out$outliers], y[out$outliers], col = "red", pch = 20)
o <- hbout(y, 10)
points(x[o], y[o], col = "green", pch = 20)

set.seed(222)
x0 <- rnorm(30, 50, 5)
x0[1] <- NA
set.seed(333)
rr <- runif(30, 0.9, 1.2)
rr[10] <- 2
x1 <- x0 * rr
x1[20] <- 0
df <- data.frame(x0, x1)
df <- df[order(df$x0),]
out <- HBmethod(yt1 = x0, yt2 = x1)
out$excluded
out$median.r
out$bounds.E
out$outliers
