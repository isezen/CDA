# http://mustafaakca.com/t-testi/
# https://acikders.ankara.edu.tr/pluginfile.php/13904/mod_resource/content/0/5.%20Tek%20Örneklem%20t-Testi.pdf
# https://stat.ethz.ch/pipermail/r-help/2008-August/170453.html
# https://datascienceplus.com/t-tests/
# https://stats.stackexchange.com/questions/141593/calculate-t-statistic-step-by-step-in-r
# https://statistics.berkeley.edu/computing/r-t-tests
# https://www.statisticssolutions.com/manova-analysis-one-sample-t-test/
library(ggplot2)
library(ggthemes)

# bonferroni test for multiple samples

ttest <- function(x, mu = 0, conf.level = 0.95) {
  x <- na.omit(x)
  m <- mean(x)
  df <- length(x) - 1
  alfa <- (1 - conf.level)/2
  se <- sd(x)/sqrt(length(x))
  t <- (m - mu)/se
  ts <- qt(c(alfa, 1 - alfa), df)
  ci <- m + ts * se
  # (ts < t)
  ret <- list(statistic = t, parameter = df, p.value = 2 * pt(-abs(t), df), conf.int = ci,
              estimate = m, null.value = mu, alternative = "two.sided",
              method = "One Sample t-test", data.name = "x")
  names(ret$statistic) <- "t"
  names(ret$parameter) <- "df"
  attr(ret$conf.int, "conf.level") <- conf.level
  names(ret$estimate) <- "mean of x"
  names(ret$null.value) <- "mean"
  class(ret) <- "htest"
  return(ret)
}

conf.level = 0.95 # confidence level
alpha <- 1 - conf.level
mu <- 7.9
# null hypo: mean is equal to mu [mu - mean(x) == 0] p-value < alpha
# alternative hypo: mean is different than mu [mu - mean(x) != 0] p-value >= alpha
n <- 10 # number of observations
df <- n - 1 # degrees of freedom
set.seed(1)
x <- rnorm(n, 10, 5) # we don't know mu and sd indeed.

(se <- sd(x)/sqrt(length(x))) # Standard Error
t <- (mean(x) - mu)/se
ts <- qt(c(alpha/2, 1 - alpha/2), df)
(ci <- mean(x) + ts * se) # confidence interval

if (t > ts[1] && t < ts[2]) {
  cat("H0: Mean is equal to mu. (Fail to reject Ho)")
} else {
  cat("H1: Mean is different than mu. (Reject Ho - do not ACCEPT H1)")
}

y <- (x - mu)/sd(x)
d <- density(y, from = -4, to = 4)[1:2]

dat <- data.frame(x = d$x, y = d$y)
dat$yt <- sapply(dat$x, function(i) dt(i, df)) # student-t distribution
dat$yn <- sapply(dat$x, function(i) dnorm(i, 0, 1)) # normal distribution

rx <- range(dat$x)
ggplot(data = dat, mapping = aes(x, y)) +
  geom_line() +
  geom_line(aes(y = yt), col = "orange",size = 1) +
  geom_line(aes(y = yn), col = "blue",size = 1) +
  geom_vline(xintercept = c(0, mean(y)), linetype = "dashed", col = c("orange", "black")) +
  geom_vline(xintercept = t, linetype = "dashed", col = "red") +
  geom_line(aes(y = 0, x = ts), as.data.frame(ts), size = 2) +
  geom_ribbon(data = subset(dat, x >= rx[1] & x <= -abs(t)), aes(ymax = yt),
              ymin = 0, fill = "red", colour = NA, alpha = 0.5) +
  geom_ribbon(data = subset(dat, x >= abs(t) & x <= rx[2]), aes(ymax = yt),
              ymin = 0, fill = "red", colour = NA, alpha = 0.5) +
  theme_tufte()

# p-value (p): Probability of obtaining a result equal to or more extreme
# than was observed in the data.
(p_val <- 2 * pt(-abs(t), df)) # p-value by student-t dist.
(p_val2 <- 2 * pnorm(-abs(t))) # p-value by normal distrbiution

# if p-value smaller than significance (alpha = 0.05), reject the null hypo.
# if p-value greater than significance, accept the null hypo.
if (p_val > alpha) {
  cat("H0: Mean is equal to mu. (Fail to reject Ho)")
} else {
  cat("H1: Mean is different than mu. (Reject Ho - do not ACCEPT H1)")
}

power.t.test(500, 1, 5, type = "one.sample")

# -------------

# For instance,
# NULL- Hypo (Ho) is "my sample is from a population that mean is 35. p-value > 0.05
# Alternative hypo (H1) is true mean is not equal to 35. p-value < 0.05
n <- 5
shape <- 10; rate = 0.25
mu <- 30; conf.level <- 0.95
set.seed(1)
s <- rgamma(n, shape, rate)
(t <- t.test(s, alternative = "t", mu = mu))

run <- 10000
y <- as.data.frame(cbind(run = 1:run, t(sapply(1:run, function(x) {
  t <- t.test(rgamma(n, shape, rate), alternative = "t",
              mu = mu, conf.level = conf.level)
  c(mean = as.numeric(t$estimate), p.value = t$p.value,
    ci1 = t$conf.int[1], ci2 = t$conf.int[2])
}))))
hist(y$p.value, breaks = seq(0, 1, 0.05))
cat("p-values lower than 0.05: %", sum(y$p.value <= 0.05)*100/run, "\n",
    "p-values greater than 0.05: %", sum(y$p.value > 0.05)*100/run, "\n")

# In t-test, the null hypothesis is that the mean of the two samples is equal.
# This means that the alternative hypothesis for the test is that the
# difference of the mean is not equal to zero.

# The t-test will also produce the p-value, which is the probability of wrongly
# rejecting the null hypothesis. Based on the result, you can say: at 95%
# confidence level, there is no significant difference (p-value = 0.0794) of
# the two means. Here you should accept the null hypothesis that the two means
# are equal because the p-value is larger than 0.05. The maximum difference of
# the mean can be as low as -3.37 and as high as 0.21.

n <- 3000; mu <- 10; s <- 5
x <- 1:10000
y <- sapply(x, function(x) {
  s1 <- rnorm(n, mu, s)
  s2 <- rnorm(n, mu, s)
  t.test(s1, s2, "t", paired = FALSE, var.equal = TRUE)$p.value
})
hist(y, breaks = seq(0, 1, 0.05))
abline(h = 0.05 * max(x))
cat("Percent of false-positive: %", sum(y <= 0.05)*100/max(x), "\n")
