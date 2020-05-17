#' @title Calculate test power
#' @description this function calculates the test power
#' (inverse of the type 2 probability).
#'
#' @param p_1 assumed population 1 p parameter
#' @param n_1 population 1 sample size
#' @param p_0 assumed population 0 p parameter
#' @param n_1 population 0 sample size
#' @param alpha test significance level
#' @param s either 1 (for one sided test) or 2 (for two sided test)
#' @param h number of hypothesis tested in the same experiment (for Bonferroni correction)
#' @param gamma minimum required lift
#'
#' @return test power
#' @example inst/power_example.R
#'
#' @details when doing one-sided tests it's usually the case
#' that population 1 is considered the treatment and population
#' population 0 serves as the control. In 2 sided tests each
#' usually represents a different treatment. 2 sided tests with
#' non zero gamma aren't supported yet.

power <- function(p_1, n_1, p_0, n_0, alpha, s, h, gamma) {
  if (gamma != 0 & s == 2) stop("using gamma != 0 with s = 2 is not supported yet")
  1 - pnorm((qnorm(1 - alpha / (s * h)) * sqrt(p_1 * (1 - p_1) / n_1 + p_0 * (1 - p_0) / n_0) -
               (p_1 - (p_0 + gamma))) / sqrt(p_1 * (1 - p_1) / n_1 + p_0 * (1 - p_0) / n_0))
}
