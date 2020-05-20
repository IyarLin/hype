#' @title Calculate test critical value
#' @description this function calculates the test statistic critical
#' value C which guarantees significance level (false discovery rate) alpha
#'
#' @param p_1_hat estimated population 1 p parameter
#' @param n_1 population 1 sample size
#' @param p_0_hat estimated population 0 p parameter
#' @param n_0 population 0 sample size
#' @param alpha test significance level
#' @param s either 1 (for one sided test) or 2 (for two sided test)
#' @param h number of hypothesis tested in the same experiment (for Bonferroni correction)
#' @param gamma minimum required lift
#'
#' @importFrom stats pnorm qnorm
#' @export
#'
#' @return critical value C
#' @example inst/critical_value_example.R
#'
#' @details when doing one-sided tests it's usually the case
#' that population 1 is considered the treatment and
#' population 0 serves as the control. In 2 sided tests each
#' usually represents a different treatment.

critical_value <- function(p_1_hat, n_1, p_0_hat, n_0, alpha, s, h, gamma) {
  if (!s %in% c(1, 2)) stop("s has to be either 1 or 2")
  if (gamma != 0 & s == 2) {
    gamma <- abs(gamma)
    s <- 1
  }
  qnorm(1 - alpha / (s * h)) *
    sqrt(p_1_hat * (1 - p_1_hat) / n_1 +
      p_0_hat * (1 - p_0_hat) / n_0) + gamma
}
