#' @title Calculate test P-value
#' @description this function calculates the test P-value
#'
#' @param p_1_hat estimated population 1 p parameter
#' @param n_1 population 1 sample size
#' @param p_0_hat estimated population 0 p parameter
#' @param n_0 population 0 sample size
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
#' @details In 2 sided tests each
#' usually represents a different treatment.
#' When doing one-sided tests it's usually the case
#' that population 1 is considered the treatment and
#' population 0 serves as the control. At any rate,
#' one sided tests are always of the form p_1 - p_0 > C.
#' For that reason for 1 sided tests one must set: gamma >= 0

p_value <- function(p_1_hat, n_1, p_0_hat, n_0, s, h, gamma) {
  # validate inputs
  if (!s %in% c(1, 2)) stop("s has to be either 1 or 2")
  if (h != round(h) | h < 1) stop("h must be a positive integer")

  # validate test logic
  if (s == 2 & gamma < 0) stop("In 2 sided tests (s=2) gamma must be equal or greater than 0")
  if (s == 2 & gamma > 0) {
    s <- 1
  }

  raw_diff <- p_1_hat - (p_0_hat + gamma)
  raw_diff[s == 2] <- abs(raw_diff)
  se <- sqrt(p_1_hat * (1 - p_1_hat) / n_1 + p_0_hat * (1 - p_0_hat) / n_0)
  z_score <- raw_diff / se
  raw_p_value <- pnorm(z_score, lower.tail = F)
  p_value <- raw_p_value * s * h
  p_value[p_value > 1] <- 1
  return(p_value)
}
