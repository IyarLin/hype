library(hype)

# For a thorugh simulation validation see
# https://github.com/IyarLin/hype/blob/master/inst/variuos_results_for_hypothesis_testing.pdf

# below we'll generate critical values for 2 hypothesis tests performed
# in the same experiemnt
# note that h is set to 2 in order to reflect that

## hypothesis test number 1
CI(
  p_1_hat = 0.212, n_1 = 10000, p_0_hat = 0.219, n_0 = 8000,
  alpha = 0.05, h = 2
)

## hypothesis test number 2
CI(
  p_1_hat = 0.11, n_1 = 5000, p_0_hat = 0.132, n_0 = 3000,
  alpha = 0.05, h = 2
)
