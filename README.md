# did_multiplegt_stat
 did_multiplegt_stat -- Estimation of heterogeneity-robust difference-in-differences (DID) estimators, with a binary, discrete, or continuous treatment or instrument, in designs with stayers, assuming that past treatments do not affect the current outcome. ([de Chaisemartin, C, D'Haultfoeuille, X, Pasquier, F, Sow, D, Vazquez-Bare, G, 2024](https://ssrn.com/abstract=4011782)).


[Description](#Description) | [Setup](#Setup) |  [Syntax](#Syntax) | [Options](#Options)

[FAQ](#FAQ) | [Example](#Example) | [References](#References) | [Authors](#Authors) | [Contact](#Contact) |


# Description

**did_multiplegt_stat** estimates difference-in-differences estimators for continuous treatments with heterogeneous effects, assuming that between consecutive periods, the treatment of some units, the switchers, changes, while the treatment of other units does not change. It computes the three estimators (including an IV-related estimator) introduced in [de Chaisemartin, C, D'Haultfoeuille, X, Pasquier, F, Sow, D, Vazquez‐Bare, G (2024)](https://ssrn.com/abstract=4011782). The estimators computed by the command assume static effects and rely on a parallel trends assumptions.

 + **Data and design.** The command uses panel data at the (G,T) level to estimate
    heterogeneity-robust DID estimators, with a binary, discrete, or continuous treatment (or
    instrument). The command can be used in designs where there is at least one pair of
    consecutive time periods between which the treatment of some units, the switchers,
    changes, while the treatment of some other units, the stayers, does not change.

 + **Target parameters.**  The command can estimate the Average Slope (AS) and the Weighted
    Average Slope (WAS) parameters introduced in de Chaisemartin et al (2022).  The AS is the
    average, across switchers, of (Y_t(D_t)-Y_t(D_{t-1})/(D_t-D_{t-1}), the effect on their
    period-t outcome of moving their period-t treatment from its period-(t-1) to its period-t
    value, scaled by the difference between these two values. The WAS is a weighted average of
    switchers' slopes (Y_t(D_t)-Y_t(D_{t-1})/(D_t-D_{t-1}), where slopes receive a weight
    proportional to |D_t-D_{t-1}|, switchers' absolute treatment change from period-(t-1) to
    period-t. The variance of the WAS estimator is often smaller than that of the AS
    estimator, especially when there are switchers that experience a small treatment change.
    The WAS estimator is also amenable to doubly-robust estimation, unlike the AS estimator.

 + **Assumptions.**  When the data has more than two time periods, the command assumes a static
    model: units' outcome at period t only depends on their period-t treatment, not on their
    lagged treatments. See the did_multiplegt_dyn command for estimators allowing for dynamic
    effects.  The command also makes a parallel trends assumption: the counterfactual outcome
    evolution switchers would have experienced if their treatment had not changed is assumed
    to be equal to the outcome evolution of stayers with the same baseline treatment.
    Importantly, this parallel-trends assumption is conditional on the baseline treatment:
    comparing switchers and stayers with different baseline treatments would implicitly amount
    to assuming that the treatment's effect is constant over time.  To test the parallel
    trends assumption underlying the estimators, the command can compute placebo estimators
    comparing the outcome evolution of switchers and stayers with the same baseline treatment
    before switchers' treatment changes.

 + **Estimators, when the exact_match option is specified.**  With a binary or discrete
    treatment, if the exact_match option is specified, the estimators computed by the command
    compare the outcome evolution of switchers and stayers with the same period-(t-1)
    treatment. Then, the WAS estimator computed by did_multiplegt_stat is numerically
    equivalent to the DID_M estimator proposed by de Chaisemartin and D'Haultfoeuille (2020),
    and already computed by the did_multiplegt_old command. did_multiplegt_stat uses an
    analytic formula to compute the estimator's variance, while did_multiplegt_old uses the
    bootstrap.  Thus, the run time of did_multiplegt_stat is typically much lower.  The
    exact_match option can only be specified when the treatment is binary or discrete:  with a
    continuously distributed treatment, one cannot find switchers and stayers with the exact
    same period-(t-1) treatment.  With a discrete treatment taking a large number of values,
    specifying this option may be undesirable:  then, there may only be few switchers that can
    be matched to a stayer with the exact same period-(t-1) treatment, thus restricting the
    estimation sample.

 + **Estimators, when the exact_match option is not specified.**  When the exact_match option is
    not specified, the command can use a regression adjustment to recover switchers'
    counterfactual outcome evolution: for all t, it runs an OLS regression of Y_t-Y_{t-1} on a
    polynomial in D_{t-1} in the sample of (t-1)-to-t stayers, and uses that regression to
    predict switchers' counterfactual outcome evolution. Alternatively, when it estimates the
    WAS, the command can also use propensity-score reweighting to recover switchers'
    counterfactual outcome evolution. First, for all t it estimates a logistic regression of
    an indicator for (t-1)-to-t switchers on a polynomial in D_{t-1}, to predict units'
    probability of being a switcher.  Then, it computes a weighted average of stayers' outcome
    evolution, upweighting stayers with a large probability of being switchers, and
    downweighting stayers with a low probability of being switchers. Finally, when it
    estimates the WAS, the command can also combine regression-adjustment and propensity-score
    reweighting, thus yielding a doubly-robust estimator.

 + **Instrumental-variable case.**  There may be instances where the parallel-trends assumption
    fails, but one has at hand an instrument satisfying a similar parallel-trends assumption.
    For instance, one may be interested in estimating the price-elasticity of a good's
    consumption, but prices respond to supply and demand shocks, and the counterfactual
    consumption evolution of units experiencing and not experiencing a price change may
    therefore not be the same. On the other hand, taxes may not respond to supply and demand
    shocks and may satisfy a parallel-trends assumption. In such cases, the command can
    compute the IV-WAS estimator introduced in de Chaisemartin et al (2022).  The IV-WAS
    estimator is equal to the WAS estimator of the instrument's reduced-form effect on the
    outcome controlling for D_{t-1}, divided by the WAS estimator of the instrument's
    first-stage effect on the treatment controlling for D_{t-1}.  See de Chaisemartin et al
    (2024) for some explanations as to why controlling for D_{t-1} is desirable in IV
    estimation.
  

# Setup

### Stata 
```s
net install did_multiplegt_stat, from("https://raw.githubusercontent.com/chaisemartinPackages/did_multiplegt_stat/main/STATA") replace
```

### R
```s
library(devtools)
install_github("chaisemartinPackages/did_multiplegt_stat/R", force = TRUE) 
```

# Syntax 

## Stata
 [**bysort varlist:**] **did_multiplegt_stat Y G T D** [**Z**] [*if*] [*in*] [, **estimator**(string) **as_vs_was
    exact_match estimation_method**(*string*) **order**(*#/####/########*) **controls**(*varlist*) **weights**(
    *varname*) **cluster**(*varlist*) **noextrapolation by_fd**(*#*) **by_baseline**(*#*) **other_treatments**(
    *varlist*) **switchers**(*string*) **placebo**(*#*) **disaggregate graph_off bys_graph_off bootstrap**(*#*)
    **seed**(*#*) **cross_validation**(*cv_suboptions*) **twfe**(*twfe_suboptions*)]

## R 


did_multiplegt_stat(df, Y, ID, Time, D, Z = NULL, estimator = NULL, estimation_method = NULL, order = 1, 
noextrapolation = FALSE, placebo = NULL,  weight = NULL, switchers = NULL, 
disaggregate = FALSE, aoss_vs_waoss = FALSE)


# Options

 + **Main options:**

    - **estimator**(*string*) gives the name(s) of the estimator(s) to be estimated. The allowed
        arguments are: (1) as, (2) was, and (3) iv-was.

    - **exact_match:** with this option, the DID estimators computed by the command compare the
        outcome evolution of switchers and stayers with the same period-(t-1) treatment (or
        instrument) value. This option can only be used when the treatment (or instrument)
        is binary or discrete:  with a continuously distributed treatment (or instrument),
        one cannot find switchers and stayers with the exact same period-(t-1) treatment (or
        instrument).  With a discrete treatment taking a large number of values, specifying
        this option may be undesirable: then, there may only be few switchers that can be
        matched to a stayer with the exact same period-(t-1) treatment, thus restricting the
        estimation sample.

    - **estimation_method**(*string*): when the exact_match option is not specified and estimation
        of the WAS or IV-WAS is requested, this option can be used to specify which
        estimation method to use when estimating the WAS or IV-WAS.  The allowed arguments
        are: (1) ra (regression adjustment), (2) ps (propensity-based reweighting), and (3)
        dr (doubly-robust).  By default, a doubly-robust estimator is used, when WAS or
        IV-WAS is requested, and the regression adjustment estimator if AS is requested.

    - **order**(*#/####/########*): when the exact_match option is not specified, this option
        specifies the polynomial orders to be used in the OLS regressions of Y_t-Y_{t-1} on
        a polynomial in D_{t-1} and/or in the logistic regressions of an indicator for
        (t-1)-to-t switchers on a polynomial in D_{t-1}.  This option allows for either 1,
        4, or 8 arguments, with 8 arguments only allowed when IV-WAS requested.  E.g.:
        order(1), order(1 4 3 2) or order(1 4 3 2 1 2 3 4) are allowed (the last one only if
        IV-WAS specified), but order(1 2 3) is not allowed.  With 4 arguments, argument 1,
        2, 3 and 4, is the order used to estimate E(Y_t-Y_{t-1}|D_{t-1}),
        P(S_{t}=0|D_{t-1}), P(S_{+, t}=1|D_{t-1}), and P(S_{-, t}=1|D_{t-1}), respectively.
        With 8 arguments the same logic is applied, but the first 4 arguments are for the
        first stage, and the next 4 for the reduced form.  Finally, if IV-WASis requested
        but order has 4 arguments, we apply the same orders to first stage and reduced form.
        By default, a polynomial of order 1 is used.
      
    - **placebo**(#): when this option is specified, the command computes the placebo version of
        each estimator requested. Actual estimators compare the t-1-to-t outcome evolution
        of period t-1-to-t switchers and stayers with the same baseline treatment. When # is
        equal to 1, placebo estimators compare the t-2-to-t-1 outcome evolution of period
        t-1-to-t switchers and stayers with the same baseline treatment, restricting
        attention to t-2-to-t-1 stayers. Thus, placebos assess whether switchers and stayers
        were on parallel trends just before switchers switched treatment. When # is strictly
        larger than 1, placebos comparing the outcome evolutions of t-1-to-t switchers and
        stayers from t-3 to t-2, from t-4 to t-3,... , and from t-#-1 to t-# are also
        reported, always restricting attention to stayers between those pairs of periods.

    - **as_vs_was**: shows a test that the AS and WAS are equal. This option can only be used when
        estimation of the AS and WAS is requested.

    - **controls**(*varlist*): the command can compute estimators with control variables. They rely
        on a conditional parallel trends assumption:  the counterfactual outcome evolution
        switchers would have experienced if their treatment had not changed is assumed to be
        equal to the outcome evolution of stayers with the same baseline treatment, and with
        the same value of varlist. When time-varying control variables are inputted to the
        command, the command compares the t-1-to-t outcome evolution of switchers and
        stayers with the same baseline treatment, and with the same controls at period t-1.
        Specifying too many control variables may lead to noisy estimators. If placebo
        estimators are small, insignificant and precisely estimated without control
        variables, including control variables may not be necessary.

    - **weights**(*varname*) : This option allows to compute estimators weighted by varname.

  + **Options to estimate heterogeneous treatment effects**

     - **switchers**(*string*): if the argument up is inputted, the command estimates the AS, WAS, or
        IV-WAS for switchers-up only, i.e for units whose treatment (or instrument)
        increases from period (t-1) to t. If the argument down is inputted, the command
        estimates the AS, WAS, or IV-WAS for switchers-down only, i.e. for units whose
        treament (or instrument) decreases from period (t-1) to t. By default, the command
        estimates those parameters for all switchers.

     - **by_fd**(*#*): This option can be used if one wants to assess the heterogeneity of the effect
        according to the absolute value of the treatment's (or instrument's) change.  For
        example, if by_fd(5) is specified, the command splits switchers into 5 groups: the
        20% with the lowest |Delta D_t| (or |Delta Z_t|), and then the next 20%, etc..  Then
        the command estimates treatment effects for each subsample. If |Delta D_t| has mass
        points, the command splits switchers into groups with as-equal-as-possible sizes.

    - **by_baseline**(*#*): This option is similar to the option by_fd(#), except that switchers are
        split into subsamples according to their values of D_{t-1} (or Z_{t-1}).

    - **[bysort varlist:]** makes did_multiplegt_stat byable. See [D] by. Only time-invariant variables are allowed in varlist.


 + **Standard-error options**

    - **bootstrap**(*#*): If the number of switchers or the number of stayers is low, one may want
        to check if the analytic standard errors produced by the command are close to
        bootstrap standard errors. If they are not, this may indicate that the asymptotic
        approximation underlying the analytic standard errors may not be reliable.  In that
        case, there is of course no guarantee that bootstrap standard errors are valid: this
        comparison is just a diagnostic check researchers may use to assess if they need to
        resort to inference methods that do not rely on asymptotic approximations, like
        permutation tests.  The bootstrap option takes as argument the number of
        replications. Currently, it is only allowed when the IV-WAS is requested, as failure
        of asymptotic approximations are more likely to arise with IV estimators, when the
        first-stage is weak.

    - **seed**(*#*): This option is only needed when one is using the bootstrap, and it allows to
        set the seed so as to ensure replicability of the results.

    - **cluster**(*varlist*) : This option allows clustering standard errors at the level of 
        varlist.

 + **Advanced options**

    - **other_treatments**(*varlist*) : This option allows controlling for other treatments (in varlist ) that may also change over the panel,
      see de Chaisemartin and D'Haultfoeuille (2021) for further details.

    - **noextrapolation**: when this option is specified, the command only keeps switchers whose
        period-(t-1) treatment (or instrument) is between the minimum and the maximum values
        of the period-(t-1) treatment (or instrument) of stayers, thus enforcing the overlap
        condition.

 + **TWFE Comparison**

    The command allows to compare the estimator specified in estimator() to a
        TWFE-estimator, computed via a regression of Y_{i,t} on D_{i,t} and unit and year
        fixed effects.  If the iv-was is specified in estimator(), a 2SLS-TWFE is used,
        using Z_{i,t} as the instrument.  With the option twfe, did_multiplegt_stat, on top
        of the main results, diplays a table showing the difference between the estimator
        requested and the TWFE-estimator, the p-value of the test of the difference and the
        corresponding condidence interval.  By default, the command runs the test using 100
        bootstrap replications.  To increase the number of replications, the user can use
        the option bootstrap(#) of did_multiplegt_stat.  Also for replicability you should
        consider using the seed(#) option.

    To use the twfe(**twfe_suboptions**) option you need to specify which sample you want to
        estimate the TWFE regression on, so you should always specify either the full_sample
        or the same_sample suboption.


    - **same_sample:** Sometimes, did_multiplegt_stat might not use all time periods
                              in the estimation.  For instance, one might have a panel data
                              where at a particular time (say p) there is no switcher. In
                              that case, did_multiplegt_stat does not use the pair of
                              periods (p-1, p). Then, the ( as, was, or iv-was) estimator
                              and the TWFE-estimator will rely on different samples.  To
                              avoid such discrepancy, the option same_sample allows to
                              estimate the TWFE-estimator using the same sample as
                              did_multiplegt_stat.
 

    - **full_sample:** Counterpart to the same_sample option. Use this in case you do
                              not want to impose the sample restrictions described in
                              same_sample and estimate the TWFE regression in the full
                              sample instead.
 

    - **percentile:**  By default, did_multiplegt_stat computes the s.e and p-value of
                              the test of the difference between the (as, was, or iv-was)
                              estimator and the TWFE-estimator using a t-test and a normal
                              approximation. Instead, one may use the percentile boostrap.
                              Then, one can specify the option percentile to compute
                              p-values and confidence intervals using the percentile
                              bootstrap method.

 + **Cross-validation**: If the treatment is continuous (or if the option exact_match is not specified), and the
        doubly-robust WAS estimator is used, instead of specifying the order of the
        polynomial series that did_multiplegt_stat uses to estimate E(Y_t-Y_{t-1}|D_{t-1}),
        P(S_{t}=0|D_{t-1}), P(S_{+, t}=1|D_{t-1}), and P(S_{-, t}=1|D_{t-1}), one may use
        cross validation.  Then, the command will choose the polynomial order with the best
        fit. This option can only be used to compute the doubly-robust WAS estimator:
        cross-validation does not have proven theoretical guarantees for the other
        estimators. This option can also not be used together with the by_fd and by_baseline
        options.
        *To use cross validation you have to specify cross_validation(algorithm(string)
        cv_suboptions).  The algorithm(string) suboption is required for the
        cross_validation(cv_suboptions) to function and has therefore to be specified in any
        case.*


    - **algorithm**(*string*): This option specifies which cross-validation algorithm to use.
                              The allowed arguments are loocv (leave-one-out) and kfolds.
                              By default, loocv is used in linear regressions and kfolds in
                              logit regressions. The leave-one-out method can only be used
                              in linear regressions.

    - **tolerance**(*#*): This option allows to set a stop criterion based on the gain in
                              prediction power.  By default, tolerance is set to 0.01, i.e.,
                              the cross-validation stops when the gain in prediction power
                              when increasing the polynomial order is less than 1%.

    - **max_k**(*#*): This is another stop criterion based on the maximum order to
                              test (the grid-search of the hyperparameter).  By default, the
                              value is set to 5, meaning that the algorithm will look for a
                              best model starting from a polynomial of order 1 to a
                              polynomial of order 5 as long as the tolerance is not reached.

    - **kfolds**(*#*): If kfolds is specified in algorithm(), this option specifies
                              the number of folds to consider.  By default, the number of
                              folds is set to 5.

    - **same_order_all_logits**: When this option is specified, the cross-validation is done for
                              only P(S_{t}=0|D_{t-1}), and the optimal order found is used
                              to predict P(S_{+, t}=1|D_{t-1}) and P(S_{-, t}=1|D_{t-1}).

    - **seed**(*#*): This option allows to set the seed so as to ensure
                              replicability of the results.

  + **Display**

    - **disaggregate**: when this option is specified, the command shows the estimated AS, WAS, or
        IV-WAS effects for each pair of consecutive time periods, on top of the effects
        aggregated across all time periods. By default, the command only shows effects
        aggregated across all time periods.

    - **graph_off**: The program displays by default a graph of the aggregated results
        (coefficients, effects and placebos, and their confidence intervals). If graph_off
        is specified, the graph is not displayed.

    - **bys_graph_off**: If the program is by'd (i.e. ran with **bysort varlist:**), or used with
        the option by_fd(#) or by_baseline(#), it automatically displays a graph of the
        aggregated results (coefficients and confidence intervals) by level of varlist, or
        quantiles, respectively. If **bys_graph_off** is specified, the graph is not displayed.



# FAQ
TBD

# Example
In the following example, we use data from Li et al. (2014). The dataset can be downloaded from the ApplicationData GitHub Repository.We first estimate the effect of gasoline taxes on gasoline consumption and prices. Then, we estimate the price-elasticity of gasoline consumption using taxes as an instrument.

## Stata
```s
use "https://github.com/chaisemartinPackages/ApplicationData/raw/main/data_gazoline.dta", clear

// Example 1 //
did_multiplegt_stat lngca id year tau, or(2) estimator(aoss waoss) estimation_method(dr) aoss_vs_waoss placebo noextra

// Example 2 //
did_multiplegt_stat lngpinc id year tau, or(2) estimator(aoss waoss) estimation_method(dr) aoss_vs_waoss placebo noextra

// Example 3 //
did_multiplegt_stat lngca id year lngpinc tau, or(2) estimator(iwaoss) estimation_method(ra) placebo noextra

```


## R
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

## Disclaimer
The ending results may vary between R and Stata (especially for the IWAOSS estimation) due to the different conventions adopted for logistic regressions by the glm and logit functions, repsectively.

# References
de Chaisemartin, C, D'Haultfoeuille, X, Pasquier, F, Sow, D, Vazquez‐Bare, G (2024). [Difference-in-Differences for Continuous Treatments and Instruments with Stayers](https://ssrn.com/abstract=4011782)

The development of this package was funded by the European Union (ERC, REALLYCREDIBLE,GA N°101043899).

# Authors

+ Clément de Chaisemartin, Economics Department, Sciences Po, France.

+ Diego Ciccia, Sciences Po, France.

+ Xavier D'Haultfoeuille, CREST-ENSAE, France.

+ Felix Knau, Sciences Po, France.

+ Felix Pasquier, CREST-ENSAE, France.

+ Doulo Sow, Sciences Po, France.

+ Gonzalo Vazquez-Bare, UCSB, USA.


# Contact
chaisemartin.packages@gmail.com
