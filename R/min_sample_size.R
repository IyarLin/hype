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
#' @details In 2 sided tests each
#' usually represents a different treatment.
#' When doing one-sided tests it's usually the case
#' that population 1 is considered the treatment and
#' population 0 serves as the control. At any rate,
#' one sided tests are always of the form p_1 - p_0 > C.
#' For that reason for 1 sided tests one must set: mde > gamma >= 0


min_sample_size <- function(mde, p_0, alpha, s, h, gamma, power) {
  # validate inputs
  if (!s %in% c(1, 2)) stop("s has to be either 1 or 2")
  if (h != round(h) | h < 1) stop("h must be a positive integer")
  if (mde <= gamma) stop("mde always has to be greater than gamma")

  # validate test logic
  if (s == 2 & gamma < 0) stop("In 2 sided tests (s=2) gamma must be equal or greater than 0")
  if (s == 2 & mde <= 0) stop("In 2 sided tests (s=2) mde must be greater than 0")

  p_1 <- p_0 + mde
  round((((qnorm(1 - alpha / (s * h)) - qnorm(1 - power)) *
            sqrt(p_1 * (1 - p_1) + p_0 *
                   (1 - p_0))) / (p_1 - (p_0 + gamma)))^2)
}
