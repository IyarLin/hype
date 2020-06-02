#' @title Calculate minimum required sample size
#' @description this function calculates the minimum
#' required sample size (equal for both treatment groups)
#' which acheives a desired MDE and power provided alpha, s,
#' h and gamma.
#'
#' @param mde minimum detectable effect
#' @param p_0 assumed population 0 p parameter
#' @param alpha test significance level
#' @param s either 1 (for one sided test) or 2 (for two sided test)
#' @param h number of hypothesis tested in the same experiment (for Bonferroni correction)
#' @param gamma minimum required lift
#' @param power desired test power (inverse of type 2 error)
#'
#' @importFrom stats pnorm qnorm
#' @export
#'
#' @return minimum requires sample size per treatment group
#' @example inst/min_sample_size_example.R
#'
#' @details when doing one-sided tests it's usually the case
#' that population 1 is considered the treatment and
#' population 0 serves as the control. In 2 sided tests each
#' usually represents a different treatment.

min_sample_size <- function(mde, p_0, alpha, s, h, gamma, power) {
  if (gamma < 0 & s == 1) stop("1 sided test (s=1) with negative minimum required lift (gamma < 0) doesn't makes sense")
  p_1 <- p_0 + mde
  round((((qnorm(1 - alpha / (s * h)) - qnorm(1 - power)) *
    sqrt(p_1 * (1 - p_1) + p_0 *
      (1 - p_0))) / (p_1 - (p_0 + gamma)))^2)
}
