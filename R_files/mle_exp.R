
library(bbmle)

# Exponantial Distribution : y = lambda * exp(-lambda * x)
ll.exp <- function(lambda, y) {
  suppressWarnings(logl <- length(y) * log(lambda) - lambda * sum(y))
  return(-logl)
}

rate <- 2
x <- rexp(1000, rate)
hist(x, probability = TRUE, las = 1, ylim = c(0, 2))
lines(density(x), col = "red") # this is not a true density function
optimize(ll.exp, interval = c(0, 5), y = x)
optim(2, ll.exp, y = x, method = "SANN")
m <- bbmle::mle2(x~dexp(rate = rate), start = list(rate = 1), data = data.frame(x))
bbmle::confint(m)
# or
lambda <- length(x)/(sum(x))
lines(sort(x), dexp(sort(x), rate), type = "l")
# ---------------------------------------------------------------------
# Poisson distribution
# 

ll.poisson <- function(mu, y) {
  logl <- sum(y) * log(mu) - length(y) * mu
  return(-logl)
}

set.seed(1)
x <- rpois(1000, 4.5)
hist(x, probability = TRUE, las = 1)
lines(density(x), col = "red") # this is not a true density function
optim(1, ll.poisson, y = x, method = "L-BFGS-B")
optimize(ll.poisson, interval = c(0, 5), y = x)
m <- bbmle::mle2(x~dpois(lambda =  lambda), start = list(lambda = 1), data = data.frame(x))
bbmle::confint(m)
# or by derivative
lambda <- mean(x)
cat("lambda=", lambda)

# ---------------------------------------------------------------------
# Normal distribution
# 
ll.norm <- function(tht, y) {
  mu <- tht[1]
  sig <- tht[2]
  suppressWarnings(
  logl <- (-0.5 * length(y) * log(2 * pi) - length(y) * log(sig) - 0.5 * sig ^ (-2) * sum((y - mu)^2))
  # or
  # logl <- sum(-0.5 * log(2 * pi) - log(sig) - 0.5 * sig ^ (-2) * (y - mu)^2)
  # or
  # logl <- sum(-log(sig) - 0.5 * sig ^ (-2) * (y - mu)^2)
  )
  return(-logl)
}

ll.norm2 <- function(tht, y) {
  mu <- tht[1]
  sig <- tht[2]
  z <- (y - mu)/sig
  logl <- sum(-log(sig) - 0.5 * z^2)
  return(-logl)
}

set.seed(3)
x <- rnorm(500, 45, 10)

ll.norm3 <- function(mu = 0.1, sig = 0.1) {
  suppressWarnings(
  -sum(dnorm(x, mu, sig, log = TRUE))
  )
}

optim(c(0.1, 0.1), ll.norm, y = x, method = "BFGS")
m <- mle(ll.norm3, method = "BFGS")

m <- bbmle::mle2(x~dnorm(mean =  mu, sd = sd), start = list(mu = 0.1, sd = 0.1), data = data.frame(x))
bbmle::confint(m)
# or by derivative
mu <- mean(x)
sdev <- sqrt((sum((x - mu)^2))/length(x))
cat("mu=", mu, " sdev = ", sdev)

# manual conf. int.
# get hessian matrix
hes.mat <- attr(m, "details")$hessian
hess <- solve(hes.mat)
prop_sigma <- sqrt(diag(hess))
prop_sigma <- diag(as.matrix(prop_sigma))
upper <- attr(m, "details")$par + 1.95 * prop_sigma
lower <- attr(m, "details")$par - 1.95 * prop_sigma
interval <- data.frame(estimated.value = attr(m, "details")$par,
                       lower = lower, upper = upper, row.names = c("mu", "sd"))
interval

# -------------------------------------
# Gamma Distribution
set.seed(3)
x <- rgamma(500, 2, 1)
ll.gamma <- function(shape, rate) {
  -sum(dgamma(x, shape, rate, log = TRUE))
}
m <- mle(ll.gamma, start = list(shape = 0.1, rate = 0.1))

hist(x, probability = TRUE, las = 1, ylim = c(0, 0.5))
lines(density(x), col = "red") # this is not a true density function
lines(sort(x), dgamma(sort(x),
                      attr(m, "details")$par[1], attr(m, "details")$par[2]),
      col = "blue")

# -------------------------------------
# Fitting a linear model

n <- 10000
x <- runif(n)
y <- 5 * x + 3 + rnorm(n, 2, 2)
fit <- lm(y ~ x)
plot(x, y); abline(fit, col = "red", lwd = 3)
#
summary(fit)

ll <- function(b0 = 0.1, b1 = 0.1, mu = 0.5, sig = 0.1) {
  R <- y - x * b1 - b0
  suppressWarnings(
    logl <- sum(dnorm(R, mu, sig, log = TRUE))
  )
  return(-logl)
}

fit2 <- mle(ll)
fit3 <- mle(ll, fixed = list(mu = 2))
