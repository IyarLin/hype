MDE <- function(power, n_1, p_0, n_0, alpha, s, h, gamma) {
  if (gamma != 0 & s == 2) stop("using gamma != 0 with s = 2 is not supported yet")
  (qnorm(1 - alpha / (s * h)) - qnorm(1 - power)) *
    sqrt(p_0 * (1 - p_0) / n_1 + p_0 * (1 - p_0) / n_0) + gamma
}