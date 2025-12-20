# Create sample settings to sample the train set.

Given a train set and a number of required outcomes, the train set
outcomes are reduced to the number of required train outcomes, trying to
maintain the original outcome to non-outcome distribution.

## Usage

``` r
createSampleTrainSetSettings(
  numberTrainSetOutcomes = NULL,
  sampleSeed = sample(10000, 1)
)
```

## Arguments

- numberTrainSetOutcomes:

  The absolute number of desired number of outcomes to retain in the
  train set.

- sampleSeed:

  A seed to use when sampling.

## Details

Create a sampleSettings object containing a number of required outcomes
to reduce the train set sample, maintaining the original outcome to
non-outcome distribution.
