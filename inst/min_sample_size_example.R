library(hype)

# For a thorugh simulation validation see
# https://github.com/IyarLin/hype/blob/master/inst/variuos_results_for_hypothesis_testing.pdf

# below we'll calculate minimum requires sample size per group
# for 2 hypothesis tests performed
# in the same experiemnt
# note that h is set to 2 in order to reflect that

## hypothesis test number 1
min_sample_size(
  mde = 0.025, p_0 = 0.2,
  alpha = 0.05, s = 1, h = 2, gamma = 0.01,
  power = 0.8
)

## hypothesis test number 2
min_sample_size(
  mde = 0.018, p_0 = 0.1,
  alpha = 0.05, s = 2, h = 2, gamma = 0,
  power = 0.8
)
