library(scales)
library(ggplot2)
library(ggthemes)

# LAW OF LARGE NUMBERS
# SAMPLING FROM SAMPLES

# Analytical Standard Error Calculation
sea <- function(sd_of_pop, sample_size) sd_of_pop/sqrt(sample_size)


nmin  <- 100; nmax <- 1e5; step <- 100
nn <- seq(nmin, nmax, step)

mulog <- 3.667024; sdlog <- 0.7
mu <- exp(mulog + (sdlog^2)/2) # Analytical solution - expected value
# analytical solution - stadard deviation
sdev <- sqrt(exp(sdlog^2 - 1) * exp(2 * mulog + sdlog^2))
set.seed(3)
x <- rlnorm(nmax, mulog, sdlog)
dfm <- as.data.frame(t(sapply(nn, function(n) {
  samp <- sample(x, n, replace = T)
  c(n = n, mean = mean(samp), std = sd(samp))
})))

dfm$SEA <- sea(sdev, dfm$n) # Analytical Solution
dfm$SEA.min <- mu - qnorm(0.975) * dfm$SEA
dfm$SEA.max <- mu + qnorm(0.975) * dfm$SEA

dfm$SEA2 <- sea(sd(x), dfm$n) # Analytical Solution
dfm$SEA2.min <- mean(x) - qnorm(0.975) * dfm$SEA2
dfm$SEA2.max <- mean(x) + qnorm(0.975) * dfm$SEA2


dfm$psize <- rescale(dfm$n, to = c(3, 0.1))
ggplot(dfm, aes(x = n)) +
  geom_point(aes(y = mean, fill = psize), shape = 21, alpha = 0.5, size = dfm$psize) +
  geom_line(aes(y = SEA.min)) + geom_line(aes(y = SEA.max)) +
  geom_line(aes(y = SEA2.min), col = "red") + geom_line(aes(y = SEA2.max), col = "red") +
  geom_hline(yintercept = mu, linetype = "dashed") +
  geom_hline(yintercept = mean(x), linetype = "dashed", color = "red") +
  scale_fill_gradient2(low = "red", mid = "white", high = "blue", midpoint = 2) +
  labs(title = "LLN (Log-Normal Dist.)", x = "Sample Size", y = "E[x]") +
  theme_tufte(base_size = 16) + theme(legend.position = "none")



mu <- 50; sdev <- 5
set.seed(5)
x <- rnorm(nmax, mu, sdev)
dfm <- as.data.frame(t(sapply(nn, function(n) {
  samp <- sample(x, n, replace = T)
  c(n = n, mean = mean(samp), std = sd(samp))
})))

dfm$SEA <- sea(sdev, dfm$n) # Analytical Solution
dfm$SEA.min <- mu - qnorm(0.975) * dfm$SEA
dfm$SEA.max <- mu + qnorm(0.975) * dfm$SEA

dfm$SEA2 <- sea(sd(x), dfm$n) # Analytical Solution from main sample
dfm$SEA2.min <- mean(x) - qnorm(0.975) * dfm$SEA2
dfm$SEA2.max <- mean(x) + qnorm(0.975) * dfm$SEA2


dfm$psize <- rescale(dfm$n, to = c(3, 0.1))
ggplot(dfm, aes(x = n)) +
  geom_point(aes(y = mean, fill = psize), shape = 21, alpha = 0.5, size = dfm$psize) +
  geom_line(aes(y = SEA.min)) + geom_line(aes(y = SEA.max)) +
  geom_line(aes(y = SEA2.min), col = "red") + geom_line(aes(y = SEA2.max), col = "red") +
  geom_hline(yintercept = mu, linetype = "dashed") +
  geom_hline(yintercept = mean(x), linetype = "dashed", color = "red") +
  scale_fill_gradient2(low = "red", mid = "white", high = "blue", midpoint = 2) +
  labs(title = "LLN (Normal Dist.)", x = "Sample Size", y = "E[x]") +
  theme_tufte(base_size = 16) + theme(legend.position = "none")
