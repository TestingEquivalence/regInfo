set.seed(123)
n <- 100; p <- 15
X <- matrix(rnorm(n * p), n, p)
beta <- c(2, 1.5, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
y <- X %*% beta + rnorm(n)

data=as.data.frame(X)
data$y=y

res_fast =knockoff.calibrate(data)

# View ranked order
res_fast$ranking

# Top 5 selected variable indices
res_fast$selected$k5

# You can later fit models like this:
X_top5 <- X[, res_fast$selected$k5, drop = FALSE]
lm(y ~ X_top5)