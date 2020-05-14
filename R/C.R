#' @title Calculate test critical value
#' @description this function calculates the test statistic critical
#' value C which guarantees significance level (false discovery rate) \alpha
#'
#' @param p_1_hat population 1 estimated p parameter
#' @param n_1 population 1 sample size
#' @param p_0_hat population 0 estimated p parameter
#' @param n_1 population 0 sample size
#' @param alpha test significance level
#' @param s either 1 (for one sided test) or 2 (for two sided test)
#' @param gamma minimum required lift
#'
#' @return critical value C
#' @example inst/example.R
#'
#' @details when doing one-sided tests it's usually the case
#' that population 1 is considered the treatment and population
#' population 0 serves as the control. In 2 sided tests each
#' usually represents a different treatment. 2 sided tests with
#' non zero gamma aren't supported yet.

C <- function(p_1_hat, n_1, p_0_hat, n_0, alpha, s, h, gamma) {
  if (gamma != 0 & s == 2) stop("using gamma != 0 with s = 2 is not supported yet")
  if (!s %in% c(1,2)) stop("s must be either 1 (for one sided test) or 2 (for two sided test)")
  qnorm(1 - alpha / (s * h)) *
    sqrt(p_1_hat * (1 - p_1_hat) / n_1 +
           p_0_hat * (1 - p_0_hat) / n_0) + gamma
}
