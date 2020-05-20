#' @title Calculate proportion difference confidence interval
#' @description this function calculates the proportion difference confidence interval.
#' This interval has probability alpha of containing the real difference.
#'
#' @param p_1_hat estimated population 1 p parameter
#' @param n_1 population 1 sample size
#' @param p_0_hat estimated population 0 p parameter
#' @param n_0 population 0 sample size
#' @param alpha confidence interval significance level
#' @param h number of hypothesis tested in the same experiment (for Bonferroni correction)
#'
#' @importFrom stats qnorm
#' @export
#'
#' @return a list with 2 elements:
#'   \item{lower}{confidence interval lower bound}
#'   \item{upper}{confidence interval upper bound}
#' @example inst/CI_example.R
#'
#' @details when doing one-sided tests it's usually the case
#' that population 1 is considered the treatment and
#' population 0 serves as the control. In 2 sided tests each
#' usually represents a different treatment.

CI <- function(p_1_hat, n_1, p_0_hat, n_0, alpha = 0.05, h) {
  point_estimate <- p_1_hat - p_0_hat
  rhs <- qnorm(1 - alpha / (2 * h)) * sqrt(p_1_hat * (1 - p_1_hat) / n_1 +
    p_0_hat * (1 - p_0_hat) / n_0)
  ans <- list(
    lower = point_estimate - rhs,
    upper = point_estimate + rhs
  )
  return(ans)
}