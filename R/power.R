#' @title Calculate test power
#' @description this function calculates the test power
#' (inverse of the type 2 probability).
#'
#' @param p_1 assumed population 1 p parameter
#' @param n_1 population 1 sample size
#' @param p_0 assumed population 0 p parameter
#' @param n_0 population 0 sample size
#' @param alpha test significance level
#' @param s either 1 (for one sided test) or 2 (for two sided test)
#' @param h number of hypothesis tested in the same experiment (for Bonferroni correction)
#' @param gamma minimum required lift
#'
#' @importFrom stats pnorm
#' @export
#'
#' @return test power
#' @example inst/power_example.R
#'
#' @details when doing one-sided tests it's usually the case
#' that population 1 is considered the treatment and
#' population 0 serves as the control. In 2 sided tests each
#' usually represents a different treatment.

power <- function(p_1, n_1, p_0, n_0, alpha, s, h, gamma) {
  if (!s %in% c(1, 2)) stop("s has to be either 1 or 2")
  if (s == 2 & gamma < 0) {
    p_0_aside <- p_0
    p_0 <- p_1
    p_1 <- p_0_aside
  }
  1 - pnorm((critical_value(p_1, n_1, p_0, n_0, alpha, s, h, gamma) -
    (p_1 - p_0)) / sqrt(p_1 * (1 - p_1) / n_1 + p_0 * (1 - p_0) / n_0))
}
