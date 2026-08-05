
<!-- README.md is generated from README.Rmd. Please edit that file -->

# semidentify

<!-- badges: start -->

[![R-CMD-check](https://github.com/zacharyvig/semidentify/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/zacharyvig/semidentify/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

`semidentify` allows the user to input a Structural Equation Model (SEM)
in [`lavaan`](https://lavaan.ugent.be/) (Rosseel, 2012) syntax and check
it against a number of identification rules from the literature. Rules
are specified as being necessary and/or sufficient and specific reasons
are given when a rule is broken. Users should not treat the package
output as the sole determinant of model identification. Instead, should
be used as a quick check for potential identification issues and
outstanding model-specification concerns.

## Installation

You can install `semidentify` from CRAN:

``` r
install.packages("semidentify")
```

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
# pak::pak("zacharyvig/semidentify")
```

## Functions

- `id()` takes a `lavaan` model string, parameter table, or model fit
  and evaluates the identification rules with informative output. The
  `call` argument is used to specify which `lavaan` function with which
  you intend to fit a model (e.g., “sem”). Output includes whether the
  rule passed, whether the rule is necessary and/or sufficient for
  identification, and, if `include.msgs = TRUE`, information about why a
  rule did or did not pass or if a rule is relevant for the particular
  type of model. Messages are classified as “Info” (information about,
  e.g., why a rule is not relevant), “Reason” (explanation of why a rule
  did not pass, but the rule was not necessary for identification), or
  “WARNING” (explanation of why a necessary rule failed).

> E.g., `id(my_model, include.msgs = TRUE, call = "sem")`

- `scaling()` prints output about how, and if so why, latent variables
  in the model are scaled. It relays which indicator is the scaling
  indicator (if applicable), whether the model has a mean structure, and
  reasons why the latent variable is or is not scaled.

> E.g., `scaling(my_model, include.msgs = TRUE, call = "cfa")`

- `id2()` evaluates the two-step rule of identification for full SEMs
  only. This rule first converts the model into a confirmatory factor
  analysis model by changing structural relationships to covariances;
  evaluates the identification of the CFA; then, if identified, converts
  the original model into a simultaneous equations model (treating
  latent variables as observed); evaluates the identification of the
  SimEM; and finally, if identified, confirms that the original model is
  identified. See Bollen’s *Elements of Structural Equation
  Models* (2026) for details.

> E.g., `id2(my_model, include.msgs = TRUE, call = "sem")`

- Note that the package supports piping for comprehensive printing,

> E.g., `id(my_model) |> scaling()` or `scaling(my_model) |> id()`

## Example

``` r
library(semidentify)
#> semidentify 0.4.0 is still in the development phase.
#> Please report any bugs or edge cases to the GitHub repository.

my_model <- ' L1 =~ x1 + x2 + x3
              L2 =~ x4 + x5 + x6
              L3 =~ x7 + x8 + x9
              L2 ~ L1
              L3 ~ L2 '

id(my_model, include.msgs = TRUE, call = "cfa", 
   meanstructure = FALSE) # check identification rules
#> Warning in id.data.frame(partable, include.msgs = include.msgs, call = call, :
#> `sem()` or `lavaan()` may be more appropriate calls for this type of model
#> semidentify 0.4.0 Rule Check
#> 
#>                        Pass Necessary Sufficient Message 
#> N_theta Rule            Yes       Yes         No 
#> Latent Scaling Rule     Yes       Yes         No 
#> Exogenous X Rule          -         -          -       1 
#> 2+ Emitted Paths Rule   Yes       Yes         No 
#> Three Indicator Rule      -         -          -       2 
#> Two Indicator Rule        -         -          -       2 
#> Fully Recursive Rule      -         -          -       3 
#> Null B_YY Rule            -         -          -       3 
#> Recur/Corr Err Rule       -         -          -       3 
#> ---
#> Messages
#> 1 - [Info] This rule only applies when causal
#>     indicators are in the model
#> 2 - [Info] This rule only applies to confirmatory
#>     factor analysis models
#> 3 - [Info] This rule only applies when there are no
#>     latent variables in the model

scaling(my_model, include.msgs = TRUE, call = "cfa", 
        meanstructure = FALSE) # check latent variable scaling
#> semidentify 0.4.0 Latent Variable Scaling
#> 
#> L1
#>   LV is scaled? Yes
#>   No. of indicators: 3
#>   Scaling indicator: x1
#>   Mean structure? No
#> 
#>   Scaling method(s):
#>   - Scaling indicator, fixed scaling-indicator
#>     intercept
#> 
#> 
#> L2
#>   LV is scaled? Yes
#>   No. of indicators: 3
#>   Scaling indicator: x4
#>   Mean structure? No
#> 
#>   Scaling method(s):
#>   - Scaling indicator, fixed scaling-indicator
#>     intercept
#> 
#> 
#> L3
#>   LV is scaled? Yes
#>   No. of indicators: 3
#>   Scaling indicator: x7
#>   Mean structure? No
#> 
#>   Scaling method(s):
#>   - Scaling indicator, fixed scaling-indicator
#>     intercept
```
