library(scales)
library(ggplot2)
library(ggthemes)

# CENTRAL LIMIT THEOREM
# SAMPLING FROM DIFFERENT DISTRIBUTIONS

subtitles1 <- c("Percent of means in Bootstrap CI: %.2f%%",
                "Percent of Bootstrap CI's covering pop. mean: %.2f%%")
subtitles2 <- c(subtitles1, "Percent of means in Base-Sample Analytical CI: %.2f%%")

# Analytical Standard Error Calculation
ase <- function(sd_of_pop, sample_size) sd_of_pop/sqrt(sample_size)
# Standard Deviation of population
pop_sd <- function(x) sqrt(sum((x - mean(x))^2)/length(x))

# Confidence Interval Calculation
CI <- function(x, cl = 0.95) {
  df <- length(x) - 1 # degrees of freedom
  se <- sd(x)/sqrt(length(x))
  alpha <- (1 - cl)/2
  z <- qt(c(alpha, 1 - alpha), df)
  mean(x) + z * se
}

# Sampling of mean from distribution
# n: Sample Size
# times: Repetation count
sampling_from_dist <- function(sampling_fun, times = 1e3, cl = 0.95) {
  n <- length(sampling_fun())
  as.data.frame(t(sapply(1:times, function(i) {
    s <- sampling_fun()
    ci <- CI(s, cl)
    c(n = n, mean = mean(s), std = sd(s), c1 = ci[1], c2 = ci[2])
  })))
}
resampling <- function(x, times = 1e3, cl = 0.95) {
  n <- length(x)
  as.data.frame(t(sapply(1:times, function(i) {
    s <- sample(x, n, replace = TRUE)
    alpha <- (1 - cl)/2
    ci <- as.vector(quantile(s, c(alpha, 1 - alpha)))
    c(n = n, mean = mean(s), std = sd(s), c1 = ci[1], c2 = ci[2])
  })))
}

# Standard Error Calculations
# dfm: Run results
# mu: Pop. Mean
# sigma: Pop. std.
# n: Sampling Size
# conf.level: Level of Confidence
SE <- function(dfm, mu, sigma, n, cl = 0.95) {
  df <- n - 1 # degrees of freedom
  alpha <- (1 - cl)/2
  z <- qt(c(alpha, 1 - alpha), df)

  # analytical SE
  ase <- sigma/sqrt(n)
  aci <- mu + z * ase

  # Numerical SE
  mean_of_means <- mean(dfm$mean)
  mean_of_sds <- mean(dfm[,3])

  pse <- sd(dfm$mean)
  pci <- quantile(dfm$mean, c(alpha, 1 - alpha))
  #
  data.frame(mean = c(mu, mean_of_means), sd = c(sigma, mean_of_sds),
             SE = c(ase, pse),
             CL = cl, n = n,
             CI.lower = c(aci[1], pci[1]),
             CI.upper = c(aci[2], pci[2]),
             row.names = c("Analytical", "Bootstrap"))
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
    geom_rect(aes(xmin = c1, xmax = c2, fill = Method, color = Method), dfci, alpha = 0.3,
              size = 0.2, ymin = -Inf, ymax = Inf, inherit.aes = FALSE) +
    geom_rug(sides = "b", col = cols, size = 0.2, length = unit(0.02, "npc")) +
    geom_vline(aes(xintercept = mu, color = Method), dfci, size = 1) +
    geom_segment(aes(x = c1, y = run, xend = c2, yend = run), alpha = 0.7,
                 color = cols, size = 0.1) +
    geom_point(col = cols, size = 0.2) +
    theme_tufte()
}

# ---------------------------------------------------

cl <- 0.999
n <- 10

# NORMAL DIST.
mu <- 30; sdev <- 10
sampling_fun <- function() rnorm(n, mu, sdev)

# By sampling
set.seed(1)
dfm <- sampling_from_dist(sampling_fun, cl = cl)
se <- SE(dfm, mu, sdev, n, cl = cl)

p <- mean_in_numerical_cis_per(dfm, se)
ci_cover <- mu_is_in_ci_per(dfm, mu)
subtitle <- make_subtitle(subtitles1, p, ci_cover)

dfci <- data.frame(Method = factor(c("Analytical", "Bootstrap")),
                   mu = se$mean, c1 = se$CI.lower, c2 = se$CI.upper)

plot_runs(dfm, dfci, mu) +
  ggtitle("Normal Dist. (Sampling)", subtitle = subtitle) +
  labs(x = "Sample Mean", y = "Run")
print(se)

# By Re-Sampling
set.seed(1)
x <- sampling_fun() # Base-Sample
ci <- CI(x) # Confidence Interval of Bas-Sample under Gaussian Assumption
dfm <- resampling(x, cl = cl)

se <- SE(dfm, mu, sdev, n, cl = cl)
se[3,] <- c(mean(x), sd(x), NA, cl, length(x), ci[1], ci[2])
rownames(se)[3] <- "Base-Sample"

p <- mean_in_numerical_cis_per(dfm, se)
ci_cover <- mu_is_in_ci_per(dfm, mu)
p2 <- 100 * sum(dfm$mean >= ci[1] & dfm$mean <= ci[2])/nrow(dfm)
subtitle <- make_subtitle(subtitles2, p, ci_cover, p2)

dfci <- data.frame(Method = factor(c("Analytical", "Bootstrap", "Base-Sample")),
                   mu = se$mean, c1 = se$CI.lower, c2 = se$CI.upper)

plot_runs(dfm, dfci, mu) +
  ggtitle("Normal Dist. (Re-Sampling)", subtitle = subtitle) +
  labs(x = "Re-Sample Mean", y = "Run")
print(se)
# -------------------------------

# lOG-NORMAL DIST.
mulog <- 3.667024; sdlog <- 0.7
# mulog <- 0.5; sdlog <- 0.7
mu <- exp(mulog + (sdlog^2)/2) # Analytical solution - expected value
# analytical solution - stadard deviation
sdev <- sqrt(exp(sdlog^2 - 1) * exp(2 * mulog + sdlog^2))
sampling_fun <- function() rlnorm(n, mulog, sdlog)

# By sampling
set.seed(1)
dfm <- sampling_from_dist(sampling_fun, cl = cl)
se <- SE(dfm, mu, sdev, n, cl = cl)

p <- mean_in_numerical_cis_per(dfm, se)
ci_cover <- mu_is_in_ci_per(dfm, mu)
subtitle <- make_subtitle(subtitles1, p, ci_cover)

dfci <- data.frame(Method = factor(c("Analytical", "Bootstrap")),
                   mu = se$mean, c1 = se$CI.lower, c2 = se$CI.upper)

plot_runs(dfm, dfci, mu) +
  ggtitle("Log-Normal Dist. (Sampling)", subtitle = subtitle) +
  labs(x = "Sample Mean", y = "Run")
print(se)

# By Re-Sampling
set.seed(1)
x <- sampling_fun() # Base-Sample
ci <- CI(x) # Confidence Interval of Bas-Sample under Gaussian Assumption
dfm <- resampling(x, cl = cl)

se <- SE(dfm, mu, sdev, n, cl = cl)
se[3,] <- c(mean(x), sd(x), NA, cl, length(x), ci[1], ci[2])
rownames(se)[3] <- "Base-Sample"

p <- mean_in_numerical_cis_per(dfm, se)
ci_cover <- mu_is_in_ci_per(dfm, mu)
p2 <- 100 * sum(dfm$mean >= ci[1] & dfm$mean <= ci[2])/nrow(dfm)
subtitle <- make_subtitle(subtitles2, p, ci_cover, p2)

dfci <- data.frame(Method = factor(c("Analytical", "Bootstrap", "Base-Sample")),
                   mu = se$mean, c1 = se$CI.lower, c2 = se$CI.upper)

plot_runs(dfm, dfci, mu) +
  ggtitle("Log-Normal Dist. (Re-Sampling)", subtitle = subtitle) +
  labs(x = "Re-Sample Mean", y = "Run")
print(se)
