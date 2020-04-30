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
  dfm$psize <- rescale(dfm$n, to = c(1, 0.01))
  dfm$fill <- rescale(dfm$n, to = c(3, 0.1))
  ggplot(dfm, aes(x = n)) +
    geom_point(aes(y = mean, fill = fill, col = fill), shape = 19, alpha = 0.5,
               size = dfm$psize) +
    geom_line(aes(y = SEA.min)) + geom_line(aes(y = SEA.max)) +
    geom_hline(yintercept = mu, linetype = "dashed") +
    scale_fill_gradient2(low = "red", mid = "darkgreen", high = "blue",
                         midpoint = 2) +
    scale_color_gradient2(low = "red", mid = "darkgreen", high = "blue",
                         midpoint = 2) +
    labs(x = "Sample Size (n)", y = "E[x]") +
    theme(legend.position = "none")
}
