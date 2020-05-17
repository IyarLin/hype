#' @title Calculate minimum required sample size
#' @description this function calculates the minimum
#' required sample size (equal for both treatment groups)
#' which acheives a desired MDE and power provided alpha, s,
#' h and gamma.
#'
#' @param p_1 assumed population 1 p parameter (can also be thought of as the MDE)
#' @param n_1 population 1 sample size
#' @param p_0 assumed population 0 p parameter
#' @param n_1 population 0 sample size
#' @param alpha test significance level
#' @param s either 1 (for one sided test) or 2 (for two sided test)
#' @param h number of hypothesis tested in the same experiment (for Bonferroni correction)
#' @param gamma minimum required lift
#' @param power desired test power (inverse of type 2 error)
#'
#' @return minimum requires sample size per treatment group
#' @example inst/min_sample_size_example.R
#'
#' @details when doing one-sided tests it's usually the case
#' that population 1 is considered the treatment and population
#' population 0 serves as the control. In 2 sided tests each
#' usually represents a different treatment. 2 sided tests with
#' non zero gamma aren't supported yet.

min_sample_size <- function(p_1, p_0, alpha, s, h, gamma, power) {
  if (gamma != 0 & s == 2) stop("using gamma != 0 with s = 2 is not supported yet")
  round((((qnorm(1 - alpha / (s * h)) - qnorm(1 - power)) *
            sqrt(p_1 * (1 - p_1) + p_0 *
                   (1 - p_0))) / (p_1 - (p_0 + gamma)))^2)
}
