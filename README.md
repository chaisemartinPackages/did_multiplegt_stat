# did_multiplegt_stat
 did_multiplegt_stat -- Estimation of heterogeneity-robust difference-in-differences (DID) estimators, with a binary, discrete, or continuous treatment or instrument, in designs with stayers, assuming that past treatments do not affect the current outcome. ([de Chaisemartin, C, D'Haultfoeuille, X, Pasquier, F, Vazquez-Bare, G, 2022](https://ssrn.com/abstract=4011782)).

## Overview

**did_multiplegt_stat** estimates difference-in-differences estimators for continuous treatments with heterogeneous effects, assuming that between consecutive periods, the treatment of some units, the switchers, changes, while the treatment of other units does not change. It computes the three estimators (including an IV-related estimator) introduced in [de Chaisemartin, C, D'Haultfoeuille, X, Pasquier, F, Vazquez‐Bare, G (2022)](https://ssrn.com/abstract=4011782). The estimators computed by the command assume static effects and rely on a parallel trends assumptions.

The command can be used with more than two periods. If the number of periods is greater than two, the command estimates, for each pair of two successive periods, the requested DID estimators (aoss, waoss or iwaoss) as well as their aggregated versions defined as a weighted average of the estimators of the different pairs of periods. The command can also be used when the panel data is unbalaced or presents gaps.

The command also computes, when the number of periods is larger than two, the placebos versions of the different estimators  for each two successive time periods, and the aggregated versions. Thus, allowing to test for parallel trends assumptions under which the proposed estimators computed by did_multiplegt_stat are unbiased.

This command can also be used when the treatment is discrete. In particular, when the treatment is discrete and takes a large number of values and the number of periods is equal to two, did_multiplegt_stat can be used as an alternative to the **did_multiplegt_dyn** command, which may not be applicable in such a design since it requires finding switchers and controls with the same period-one treatment. When the number of periods is larger than two, the two commands estimate two different models (static effects for **did_multiplegt_stat**, and dynamic effects for **did_multiplegt_dyn**).

## Setup

### Stata 
```s
net install did_multiplegt_stat, from("https://raw.githubusercontent.com/chaisemartinPackages/did_multiplegt_stat/main/Stata") replace
```

### R
```s
library(devtools)
install_github("chaisemartinPackages/did_multiplegt_stat/R", force = TRUE) 
```

## Syntax 

### Stata
```r
[bysort varlist:] did_multiplegt_stat Y G T D [Z] [if] [in] [, estimator(string) as_vs_was exact_match estimation_method(string) order(#) controls(varlist) 
    weights(varname) cluster(varlist) noextrapolation by_fd(#) by_baseline(#) other_treatments(varlist) switchers(string) placebo(#) disaggregate graph_off bys_graph_off
    bootstrap(#) seed(#) cross_validation(cv_suboptions) twfe(twfe_suboptions)]
```

### R 
```r
did_multiplegt_stat(df, Y, ID, Time, D, Z = NULL, estimator = NULL, estimation_method = NULL, order = 1, 
noextrapolation = FALSE, placebo = NULL,  weight = NULL, switchers = NULL, 
disaggregate = FALSE, aoss_vs_waoss = FALSE)
```

## Description

- **df**: (R only) A dataframe object.
- **Y**: Outcome variable.
- **ID**: Identifier of the unit of analysis.
- **Time**: Time variable. The command assumes that the time variable is evenly spaced (e.g.: the panel is at the yearly level, and no year is missing for all groups). When it is not (e.g.: the panel is at the yearly level, but three consecutive years are missing for all groups), the command can still be used. For example, if the year n is missing, the command does not comuptes the DID estimators of the pairs of years (n-1,n),(n,n+1), and (n-1,n+1).
- **D**: Treatment variable.
- **Z**: Instrumental variable. This option is only required when the IV-related estimator (the so-called iwaoss) is requested.
- **estimator**: Estimator(s) to be computed. The allowed arguments are: (1) "aoss", i.e the Average Of Switchers’ Slopes which is the average, across switchers, of the effect on their period-(t) outcome of moving their treatment from its period-(t-1) to its period-(t) value, scaled by the difference between these two values. (2) "waoss" which corresponds to a weighted version of "aoss" where slopes receive a weight proportional to switchers’ absolute treatment change from period-(t-1) to period-(t). (3) "iwaoss" which generalizes "waoss" to the instrumental-variable case, and is equal to the reduced-form "waoss" effect of the instrument on the outcome, divided by the first-stage "waoss" effect of the instrument on the treatment. If this option is not specified: by default, the command estimates both "aoss" and "waoss" if the instrumental-variable Z is not specified, or only iwaoss otherwise. 
- **estimation_method**: This option allows to specify which estimation method to use when estimating the waoss or the iwaoss, as described in de Chaisemartin, C, D'Haultfoeuille, X, Pasquier, F, Vazquez‐Bare, G (2022). It takes as argument "ra" (regression adjustment-based approach), or "ps" (propensity-based approach), or "dr" (double robust-based approach).
- **order**: This option takes as argument the order of the polynomial series used to estimate the counterfactual of the variation of the outcome from period t-1 to period t  for the switchers, namely $E(Y_t - Y_{t-1} |D_{t-1}, S_t = 0)$ or $E(Y_t - Y_{t-1} |Z_{t-1}, SI_t = 0)$. 
- **switchers**: The allowed inputs for this option are "up" and "down". If the argument "up" is specified, the command estimates the effects on switchers-up, i.e, units whose treatments (or instruments) increase from period t-1 to period t. If the argument "down" is given, the command estimates the effects on switchers-down, i.e., units whose treaments (or instruments) decrease from period t-1 to period t.
- **disaggregate**: If this potion is specified, the command displays the estimands of the effects for each two consecutive periods as well as the aggregated estimands. Otherwise, the command only outputs the aggregated results.
- **placebo**: This option allows to estimate the placebos versions of the estimators requested in the estimator option. If this option is combined with the option disaggregate, the command also displays the placebo version of each two consecutive time-periods.
- **weight**: This option allows the user to specify weights for the three estimation methods.
- **noextrapolation**: This option forces the command to use only switchers whose period-(t-1) treatments (or instruments) are between the minimum and the maximum values of the period-(t-1) treatments (or instruments) of the stayers. This a less restrictive common support assumption.
- **aoss_vs_waoss**: As highlighted in de Chaisemartin, C, D'Haultfoeuille, X, Pasquier, F, Vazquez‐Bare, G (2022), the aoss and the waoss are equal if and only if switchers’ slopes are uncorrelated with $|D_t - D_{t-1}|$. When this option is specified, the command performs and displays the test of the equality between the aoss and  the waoss. Note that the use of this option requires specifying in the estimator option both aoss and waoss.

## FAQ:
TBD

## Example
In the following example, we use data from Li et al. (2014). The dataset can be downloaded from the ApplicationData GitHub Repository.We first estimate the effect of gasoline taxes on gasoline consumption and prices. Then, we estimate the price-elasticity of gasoline consumption using taxes as an instrument.

### Stata
```s
use "https://github.com/chaisemartinPackages/ApplicationData/raw/main/data_gazoline.dta", clear

// Example 1 //
did_multiplegt_stat lngca id year tau, or(2) estimator(aoss waoss) estimation_method(dr) aoss_vs_waoss placebo noextra

// Example 2 //
did_multiplegt_stat lngpinc id year tau, or(2) estimator(aoss waoss) estimation_method(dr) aoss_vs_waoss placebo noextra

// Example 3 //
did_multiplegt_stat lngca id year lngpinc tau, or(2) estimator(iwaoss) estimation_method(ra) placebo noextra

```


### R
```s
library(haven)
gazoline <-  haven::read_dta("https://github.com/chaisemartinPackages/ApplicationData/raw/main/data_gazoline.dta")

# Example 1
summary(did_multiplegt_stat(df = gazoline, Y = "lngca", ID = "id", T = "year", D = "tau", order = 2, estimator = c("aoss", "waoss"), estimation_method = "dr", aoss_vs_waoss = TRUE, placebo = TRUE, noextrapolation = TRUE))

# Example 2
summary(did_multiplegt_stat(df = gazoline, Y = "lngpinc", ID = "id", T = "year", D = "tau", order = 2, estimator = c("aoss", "waoss"), estimation_method = "dr", aoss_vs_waoss = TRUE, placebo = TRUE, noextrapolation = TRUE))

# Example 3
summary(did_multiplegt_stat(df = gazoline, Y = "lngca", ID = "id", T = "year", D = "lngpinc", Z = "tau", order = 2, estimator = "iwaoss", estimation_method = "ra", placebo = TRUE, noextrapolation = TRUE))
```

### Disclaimer
The ending results may vary between R and Stata (especially for the IWAOSS estimation) due to the different conventions adopted for logistic regressions by the glm and logit functions, repsectively.

## References:
de Chaisemartin, C, D'Haultfoeuille, X, Pasquier, F, Vazquez‐Bare, G (2022). [Difference-in-Differences for Continuous Treatments and Instruments with Stayers](https://ssrn.com/abstract=4011782)

The development of this package was funded by the European Union (ERC, REALLYCREDIBLE,GA N°101043899).

## Contact
chaisemartin.packages@gmail.com