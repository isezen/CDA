library(scales)
library(ggplot2)
library(ggthemes)

# LAW OF LARGE NUMBERS
# SAMPLING FROM DIFFERENT DISTRIBUTIONS

# Analytical Standard Error Calculation
sea <- function(sd_of_pop, sample_size) sd_of_pop/sqrt(sample_size)

# Standard Error Analytical Solution for each sample size (n)
sea2 <- function(dfm, mu_of_pop, sd_of_pop) {
  dfm$SEA <- sea(sd_of_pop, dfm$n)
  dfm$SEA.min <- mu_of_pop - qnorm(0.975) * dfm$SEA
  dfm$SEA.max <- mu_of_pop + qnorm(0.975) * dfm$SEA
  return(dfm)
}
# Calcualte Sample statistics
sample_stats <- function(n, dist_fun, ...) {
  set.seed(n)
  s <- dist_fun(n, ...)
  c(n = n, mean = mean(s), std = sd(s))
}

calc_sample_means <- function(...) {
  nn <- (1:1000) * 100
  as.data.frame(t(sapply(nn, sample_stats, ...)))
}

plot_lln <- function(dfm, mu, str_dist) {
  dfm$psize <- rescale(dfm$n, to = c(3, 0.1))
  ggplot(dfm, aes(x = n)) +
    geom_point(aes(y = mean, fill = psize), shape = 21, alpha = 0.5,
               size = dfm$psize) +
    geom_line(aes(y = SEA.min)) + geom_line(aes(y = SEA.max)) +
    geom_hline(yintercept = mu, linetype = "dashed") +
    scale_fill_gradient2(low = "red", mid = "white", high = "blue",
                         midpoint = 2) +
    labs(title = sprintf("LLN (%s Dist.)", str_dist), x = "Sample Size",
         y = "E[x]") +
    theme_tufte(base_size = 16) + theme(legend.position = "none")
}

# --------------------------------------------------------
# Normal Dist.
mu <- 10; sdev <- 5
dfm <- calc_sample_means(rnorm, mu, sdev)
dfm <- sea2(dfm, mu, sdev) # Standard Error Analytical Solution
plot_lln(dfm, mu, "Normal")

# log-normal dist.
mulog <- 0.5; sdlog <- 0.7
mu <- exp(mulog + (sdlog^2)/2) # Analytical solution - expected value
# analytical solution - stadard deviation
sdev <- sqrt(exp(sdlog^2 - 1) * exp(2 * mulog + sdlog^2))
dfm <- calc_sample_means(rlnorm, mulog, sdlog)
dfm <- sea2(dfm, mu, sdev) # Standard Error Analytical Solution
plot_lln(dfm, mu, "Log-Normal")

# Gamma Dist.
shape <- 10; rate <- 2; scale <- 1/rate
mu <- (shape * scale) # Analytical solution - expected value
sdev <- sqrt((shape * scale^2)) # analytical solution - stadard deviation
dfm <- calc_sample_means(rgamma, shape, rate)
dfm <- sea2(dfm, mu, sdev) # Standard Error Analytical Solution
plot_lln(dfm, mu, "Gamma")

# Weibull Dist.
shape <- 10; scale <- 2
mu <- (scale * gamma(1 + 1/shape)) # Analytical solution - expected value
# analytical solution - stadard deviation
sdev <- sqrt((scale^2 * (gamma(1 + 2/shape) - gamma(1 + 1/shape)^2)))
dfm <- calc_sample_means(rweibull, shape, scale)
dfm <- sea2(dfm, mu, sdev) # Standard Error Analytical Solution
plot_lln(dfm, mu, "Weibull")
