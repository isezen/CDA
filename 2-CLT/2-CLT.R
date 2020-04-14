library(scales)
library(ggplot2)
library(ggthemes)
library(boot)

# CENTRAL LIMIT THEOREM
# RE-SAMPLING FROM DISTRIBUTIONS

subtitles1 <- c("Percent of CI's covering pop. mean: %.2f%%")

# Analytical Standard Error Calculation
ase <- function(sd_of_pop, sample_size) sd_of_pop/sqrt(sample_size)
# Standard Deviation of population
pop_sd <- function(x) sqrt(sum((x - mean(x))^2)/length(x))

# Confidence Interval Calculation
CI <- function(x, se = NA, cl = 0.95) {
  df <- length(x) - 1 # degrees of freedom
  if (is.na(se)) se <- sd(x)/sqrt(length(x))
  alpha <- (1 - cl)/2
  mean(x) + qt(c(alpha, 1 - alpha), df) * se
}

# Sampling of mean from distribution
# n: Sample Size
# times: Repetation count
resampling_from_dist <- function(sampling_fun, times = 1e2,
                                 resampling.times = 1e2, cl = 0.95) {
  n <- length(sampling_fun())
  as.data.frame(t(sapply(1:times, function(i) {
    x <- sampling_fun() # pick a sample
    means <- sapply(1:resampling.times, function(i) {
      mean(sample(x, length(x), replace = TRUE))
    })
    se <- sd(means) # SE is equal to SD of means of re-sampled values
    alpha <- (1 - cl)/2
    ci <- quantile(means, c(alpha, 1 - alpha))
    c(n = n, rs.times = resampling.times, mean = mean(x), sd = sd(x),
      se = se, c1 = ci[1], c2 = ci[2])
  })))
}

resampling_from_dist2 <- function(sampling_fun, times = 1e2,
                                  resampling.times = 1e4, cl = 0.95) {
  n <- length(sampling_fun())
  if (is.na(resampling.times)) resampling.times <- n
  as.data.frame(t(sapply(1:times, function(i) {
    boot_mean <- function(x, indices) mean(x[indices])
    x <- sampling_fun() # pick a sample
    res <- boot(data = x, statistic = boot_mean, R = resampling.times)
    se <- sd(res$t)
    alpha <- (1 - cl)/2
    ci <- as.vector(quantile(res$t, c(alpha, 1 - alpha)))
    c(n = n, rs.times = resampling.times, mean = mean(x), sd = sd(x),
      se = se, cl = cl, c1 = ci[1], c2 = ci[2])
  })))
}

# Standard Error Calculations
# mu: Pop. Mean
# sigma: Pop. std.
# n: Sampling Size
# conf.level: Level of Confidence
SE <- function(mu, sigma, n, cl = 0.95) {
  df <- n - 1 # degrees of freedom
  alpha <- (1 - cl)/2
  z <- qt(c(alpha, 1 - alpha), df)

  # analytical SE
  ase <- sigma/sqrt(n)
  aci <- mu + z * ase
  data.frame(mean = mu, sd = sigma,
             SE = ase, CL = cl, n = n,
             CI.lower = aci[1], CI.upper = aci[2],
             row.names = "Analytical")
}

mean_in_numerical_cis_per <- function(dfm, se) {
  100 * sum(dfm$mean >= se$CI.lower[2] & dfm$mean <= se$CI.upper[2])/nrow(dfm)
}
mu_is_in_ci <- function(dfm, mu) mu > dfm$c1 & mu < dfm$c2
mu_is_in_ci_per <- function(dfm, mu) 100 * sum(mu_is_in_ci(dfm, mu))/nrow(dfm)

make_subtitle <- function(str, ...)
  sprintf(paste0(str, collapse = "\n"), ...)

plot_runs <- function(dfm, dfci, mu) {
  df2 <- data.frame(run = 1:nrow(dfm), mean = dfm$mean, c1 = dfm$c1, c2 = dfm$c2)
  df2$is_in <- mu_is_in_ci(dfm, mu)

  cols <- c("darkred", "darkgreen")[as.numeric(df2$is_in) + 1]
  ggplot(df2, aes(x = mean, y = run)) +
    geom_rect(aes(xmin = c1, xmax = c2, fill = Method, color = Method), dfci, alpha = 0.1,
              size = 0.2, ymin = -Inf, ymax = Inf, inherit.aes = FALSE) +
    geom_rug(sides = "b", col = cols, size = 0.2, length = unit(0.02, "npc")) +
    geom_vline(aes(xintercept = mu, color = Method), dfci, size = 1) +
    geom_segment(aes(x = c1, y = run, xend = c2, yend = run), alpha = 0.9,
                 color = cols, size = 0.1) +
    geom_point(col = cols, size = 0.2) +
    theme_tufte()
}

cl <- 0.99
n <- 100

# NORMAL DIST.
mu <- 30; sdev <- 10
sampling_fun <- function() rnorm(n, mu, sdev)

set.seed(1)
dfm <- resampling_from_dist(sampling_fun, times = 1000, cl = cl)
(se <- SE(mu, sdev, n, cl = cl))

subtitle <- make_subtitle(subtitles1, mu_is_in_ci_per(dfm, mu))

dfci <- data.frame(Method = factor(c("Analytical")),
                   mu = se$mean, c1 = se$CI.lower, c2 = se$CI.upper)

plot_runs(dfm, dfci, mu) +
  ggtitle("Normal Dist. (Re-Sampling from samples)", subtitle = subtitle) +
  labs(x = "Sample Mean", y = "Run")

# -------------------------------

# lOG-NORMAL DIST.
# mulog <- 3.667024; sdlog <- 0.7
mulog <- 0.5; sdlog <- 0.7
mu <- exp(mulog + (sdlog^2)/2) # Analytical solution - expected value
# analytical solution - stadard deviation
sdev <- sqrt(exp(sdlog^2 - 1) * exp(2 * mulog + sdlog^2))
sampling_fun <- function() rlnorm(n, mulog, sdlog)

# By sampling
set.seed(1)
dfm <- resampling_from_dist2(sampling_fun, times = 1e3, resampling.times = 1e2, cl = cl)
(se <- SE(mu, sdev, n, cl = cl))

subtitle <- make_subtitle(subtitles1, mu_is_in_ci_per(dfm, mu))

dfci <- data.frame(Method = factor(c("Analytical")),
                   mu = se$mean, c1 = se$CI.lower, c2 = se$CI.upper)

plot_runs(dfm, dfci, mu) +
  ggtitle("Log-Normal Dist. (Re-Sampling from samples)", subtitle = subtitle) +
  labs(x = "Sample Mean", y = "Run")
