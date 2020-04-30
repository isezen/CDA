library(isofor)
library(solitude)

# https://cs.nju.edu.cn/zhouzh/zhouzh.files/publication/icdm08b.pdf
N <- 1e3
x <- c(rnorm(N, 0, 0.5), rnorm(N*0.05, -1.5, 1))
y <- c(rnorm(N, 0, 0.5), rnorm(N*0.05,  1.5, 1))
ol <- c(rep(0, N), rep(1, (0.05*N))) + 2
data <- data.frame(x, y)
plot(data, pch = ol, main = "Dummy data with outliers")

# isofor
max.depth <- 7
nt <- 1e3; seed <- 1; phi <- 2^max.depth; prob <- 0.95
mod <- isofor::iForest(X = data, nt = nt, phi = phi, seed = seed)
p <- predict(mod, data)
tb <- table(iForest = p > quantile(p, prob), Actual = ol == 3)
print(tb)
cat("Success: %", (tb[1] + tb[4]) * 100/sum(tb),
    "\nFailure: %", (tb[2] + tb[3]) * 100/sum(tb), "\n")
col <- ifelse(p > quantile(p, prob), "red", "blue")
plot(x, y, col = col, pch = ol)


# solitude
max.depth <- 7
nt <- 1e4; seed <- 1; phi <- 2^max.depth; prob <- 0.95
mod2 <- solitude::isolation_forest(data, seed = seed, num.trees = nt,
                                   max.depth = max.depth)
p2 <- predict(mod2, data, type = "anomaly_score")
tb <- table(isolation_forest = p2 > quantile(p2, prob), Actual = ol == 3)
print(tb)
cat("Success: %", (tb[1] + tb[4]) * 100/sum(tb),
    "\nFailure: %", (tb[2] + tb[3]) * 100/sum(tb), "\n")
col <- ifelse(p2 > quantile(p2, prob), "red", "blue")
plot(x, y, col = col, pch = ol)

depths <- predict(mod2, data, type = "depth_corrected") # predict corrected depths
quantile(p2, probs = seq(0.5, 1, length.out = 11)) # quantiles of anomaly scores
image(1:nrow(depths), 1:ncol(depths), depths) # visualize depths
