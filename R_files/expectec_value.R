# Expected Value examples

exp_val <- function(FUN, ...) integrate(function(x) x * FUN(x, ...), -Inf, Inf)$value
Ex2 <- function(FUN, ...) integrate(function(x) x^2 * FUN(x, ...), -Inf, Inf)$value
format_str <- paste("*** %s ***",
                    "Exp.Val: %f; STD: %f",
                    "Analyt. exp. val: %f; Analyt. STD.: %f",
                    "Sample Mean: %f; Sample STD: %f", sep = "\n")

# Normal Dist.
n <- 100
mu <- 10; sdev <- 5
x <- rnorm(n, mu, sdev)
d <- density(sort(x))
ev <- exp_val(dnorm, mu, sdev) # expected value by integral
esd <- sqrt(Ex2(dnorm, mu, sdev) - ev^2) # standart deviation by integral
m <- mean(x) # Mean of sample
s <- sd(x) # Standard deviation of sample
cat(sprintf(format_str, "Normal Dist", ev, esd, mu, sdev, m, s))

# Gamma Dist.
n <- 10
shape <- 10; rate <- 2; scale <- 1/rate
x <- rgamma(n, shape, rate)
ev <- exp_val(dgamma, shape, rate) # expexted value by integral
aev <- (shape * scale) # Analytical solution - expected value
esd <- sqrt(Ex2(dgamma, shape, rate) - ev^2) # Standard Deviation by integral
asd <- sqrt((shape * scale^2)) # analytical solution - stadard deviation
m <- mean(x)
s <- sd(x)
cat(sprintf(format_str,"Gamma Dist.", ev, esd, aev, asd, m, s))

# Weibull Dist.
n <- 10
shape <- 10; scale <- 2
x <- rweibull(n, shape, scale)
ev <- exp_val(dweibull, shape, scale) # expexted value by integral
aev <- (scale * gamma(1 + 1/shape)) # Analytical solution - expected value
esd <- sqrt(Ex2(dweibull, shape, scale) - ev^2) # standard deviation by integral
asd <- sqrt((scale^2 * (gamma(1 + 2/shape) - gamma(1 + 1/shape)^2))) # Analytical solution - expected value
m <- mean(x) # mean of sample
s <- sd(x) # standard deviation of sample
cat(sprintf(format_str, "Weibull Dist", ev, esd, aev, asd, m, s))