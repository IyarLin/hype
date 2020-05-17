library(hype)

# For a thorugh simulation validation see
# https://github.com/IyarLin/hype/blob/master/inst/variuos_results_for_hypothesis_testing.pdf

# below we'll calculate power for 2 hypothesis tests performed
# in the same experiemnt
# note that h is set to 2 in order to reflect that

## hypothesis test number 1
power(p_1 = 0.225, n_1 = 10000, p_0 = 0.2, n_0 = 8000,
  alpha = 0.05, s = 1, h = 2, gamma = 0.01)

## hypothesis test number 2
power(p_1 = 0.118, n_1 = 5000, p_0 = 0.1, n_0 = 3000,
  alpha = 0.05, s = 2, h = 2, gamma = 0)
