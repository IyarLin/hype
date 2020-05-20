#' @title Calculate test Minimum detectable effect (MDE)
#' @description this function calculates the test minimum
#' dedtectable effect (MDE). This is the smallest effect
#' size p_1 - p_0 which yields the desired power and provided
#' sample sizes, alpha, s, h and gamma.
#'
#' @param n_1 population 1 sample size
#' @param p_0 assumed population 0 p parameter
#' @param n_0 population 0 sample size
#' @param alpha test significance level
#' @param s either 1 (for one sided test) or 2 (for two sided test)
#' @param h number of hypothesis tested in the same experiment (for Bonferroni correction)
#' @param gamma minimum required lift
#' @param power desired test power (inverse of type 2 error)
#'
#' @importFrom stats pnorm qnorm
#' @export
#'
#' @return test MDE
#' @example inst/mde_example.R
#'
#' @details when doing one-sided tests it's usually the case
#' that population 1 is considered the treatment and
#' population 0 serves as the control. In 2 sided tests each
#' usually represents a different treatment.

MDE <- function(power, n_1, p_0, n_0, alpha, s, h, gamma) {
  if (!s %in% c(1, 2)) stop("s has to be either 1 or 2")
  if (s == 2) gamma <- abs(gamma)
  (qnorm(1 - alpha / (s * h)) - qnorm(1 - power)) *
    sqrt(p_0 * (1 - p_0) / n_1 + p_0 * (1 - p_0) / n_0) + gamma
}
