library(ggplot2)
library(ggthemes)
library(MASS)

# Generate gamma rvs
set.seed(1)
y <- NULL # dummy var
x <- rgamma(10000, shape = 2, rate = 0.2)
den <- density(x)
dat <- data.frame(x = den$x, y = den$y)

# Plot density
ggplot(dat, aes(x, y)) + geom_line(size = 1) +
  theme_tufte(base_size = 16)

# Fit parameters (to avoid errors, set lower bounds to zero)
fit <- fitdistr(x, "gamma", lower = c(0, 0))
est <- fit$estimate

# Plot using density points + histogram
ggplot(dat, aes(x, y)) +
  geom_histogram(aes_string("x", "..density.."), as.data.frame(x),
                 binwidth = 0.1, col = "black", fill = "pink") +
  geom_point(size = 2) +
  geom_line(aes(x, y = dgamma(x, est["shape"], est["rate"])),
            color = "red", size = 1) + theme_tufte(base_size = 16)


h <- hist(x, 1000, plot = T, freq = F)
t1 <- data.frame(x = h$mids, y = h$density)
ggplot(t1, aes(x, y)) + geom_point(size = 2) +
  geom_line(aes(x, dgamma(x, est["shape"], est["rate"])),
            color = "red", size = 1) + theme_tufte(base_size = 16)
