
<!-- README.md is generated from README.Rmd. Please edit that file -->

# poc.occupational.insurance

<!-- badges: start -->
<!-- badges: end -->

The goal of poc.occupational.insurance is to give a proof of concept of
a 3 state Markov Model to price classical insurance products for
disability/occupational insurance. A sample data set by the bank of
St. Louis from 1977 is also included. This is a toy model, so if you
want to stay solvent rather build on it instead of using it directly.
Also it is a personal playground for using `R`.

## Installation

You can install the development version of poc.occupational.insurance
like so:

``` r
# install.packages("devtools")
devtools::install_github("n-0/poc.occupational.insurance")
```

## Example

If not directly supplying a table of transition probabilities (for the
columns take a look at `?determine_probs`), one can parse them from a
data set, such as the one included. The example plots the part for
occupied insurees of the actuarial reserves with a policy of 20 years a
yearly benefit of 500 monetary units and a constant premium paid in
advance. The interest is assumed to be constant with `3%`.

``` r
library(poc.occupational.insurance)

## Plots the example 
example_of_reserve_plot = function() {
    transition_probs = determine_probs(occupation1977stlouis)
    duration = 20 
    benefit = 500
    premium_duration = 5
    v = 1/1.03
    x = 30
    ts = c(0:(duration-1))
    active_reserve = sapply(ts, function(t) occupation_insurance_recurrent_reserve(
         t, x, duration, benefit, v, transition_probs, active=TRUE, opt_premium_duration=premium_duration
         )
    )
    barplot(active_reserve)
    active_reserve
}
```
