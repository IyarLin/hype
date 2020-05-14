library(hype)

p_0 <- 0.2
p_1 <- 0.2 # Null is true
n_0 <- n_1 <- 10000
alpha <- 0.05

s <- 1 # one sided test
h <- 3 # number of tests
gamma <- 0.02 # minimum required lift
M <- 10000 # number of simulations

p_1_hat <- as.matrix(replicate(
  n = h,
  rbinom(M, size = n_1, prob = p_1 + gamma) / n_1
), ncol = h)

p_0_hat <- as.matrix(replicate(
  n = h,
  rbinom(M, size = n_0, prob = p_0) / n_0
), ncol = h)
const <- mapply(
  function(p_1_hat, p_0_hat) {
    C(
      p_1_hat = p_1_hat, n_1 = rep(n_1, M),
      p_0_hat = p_0_hat, n_0 = rep(n_0, M),
      alpha = alpha, s = s, h = h,
      gamma = gamma
    )
  },
  split(p_1_hat, rep(1:ncol(p_1_hat),
                     each = nrow(p_1_hat)
  )),
  split(p_0_hat, rep(1:ncol(p_0_hat),
                     each = nrow(p_0_hat)
  ))
)

diff <- p_1_hat - p_0_hat

mean(apply(diff - const, 1, function(row) any(row > 0))) # note the any function - pertaining to FWER
