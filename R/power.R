power <- function(p_1, n_1, p_0, n_0, alpha, s, h, gamma) {
  if (gamma != 0 & s == 2) stop("using gamma != 0 with s = 2 is not supported yet")
  1 - pnorm((qnorm(1 - alpha / (s * h)) * sqrt(p_1 * (1 - p_1) / n_1 + p_0 * (1 - p_0) / n_0) -
               (p_1 - (p_0 + gamma))) / sqrt(p_1 * (1 - p_1) / n_1 + p_0 * (1 - p_0) / n_0))
}