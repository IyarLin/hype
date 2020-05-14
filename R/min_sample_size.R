min_sample_size <- function(p_1, p_0, alpha, s, h, gamma, power) {
  if (gamma != 0 & s == 2) stop("using gamma != 0 with s = 2 is not supported yet")
  round((((qnorm(1 - alpha / (s * h)) - qnorm(1 - power)) *
            sqrt(p_1 * (1 - p_1) + p_0 *
                   (1 - p_0))) / (p_1 - (p_0 + gamma)))^2)
}