{smcl}
{* *! version 1  2024-01-29}{...}
{viewerjumpto "Syntax" "did_multiplegt_stat##syntax"}{...}
{viewerjumpto "Description" "did_multiplegt_stat##description"}{...}
{viewerjumpto "Options" "did_multiplegt_stat##options"}{...}
{viewerjumpto "Examples" "did_multiplegt_stat##examples"}{...}
{viewerjumpto "Saved results" "did_multiplegt_stat##saved_results"}{...}

{title:Title}

{p 4 4}
{cmd:did_multiplegt_stat} {hline 2} Estimation of heterogeneity-robust difference-in-differences (DID) estimators, 
with a binary, discrete, or continuous treatment or instrument, in designs with stayers.
{p_end}

{marker syntax}{...}
{title:Syntax}

{p 4 4}
[{opt bys:ort} {varlist}{cmd::}]
{cmd:did_multiplegt_stat Y ID T D [Z]} {ifin}
[{cmd:,}
{cmd:estimator(}{it:string}{cmd:)}
{cmd:aoss_vs_waoss}
{cmd:exact_match}
{cmd: estimation_method(}{it:string}{cmd:)}
{cmd:{ul:or}der(}{it:#}{cmd:)}
{cmd:controls({varlist}{cmd:})}
{cmd:{ul:noextra}polation}
{cmd:by_fd(}{it:#}{cmd:)}
{cmd:bys_baseline(}{it:#}{cmd:)}
{cmd:other_treatments({varlist}{cmd:})}
{cmd:cluster({varlist}{cmd:})}
{cmd: switchers(}{it:string}{cmd:)}
{cmd:placebo}
{cmd:{ul:disag}gregate}
{cmd:bys_graph_off}
{cmd:bootstrap({it:#})}
{cmd:seed({it:#})}
{p_end}

{p 4 4}
{cmd:Y} is the outcome variable.
{p_end}

{p 4 4}
{cmd:ID} is the identifier of the unit of analysis.
{p_end}

{p 4 4}
{cmd:T} is the time period variable.
{p_end}

{p 4 4}
{cmd:D} is the treatment variable.
{p_end}

{p 4 4}
{cmd:Z} (optional) is an instrumental variable.
{p_end}

{synoptset 28 tabbed}{...}

{marker description}{...}
{title:Description}

{p 4 4}
{cmd:Data and design.} The command uses panel data at the {cmd:(ID,T)} level to estimate heterogeneity-robust DID estimators, 
with a binary, discrete, or continuous treatment (or instrument). The command can be used in designs where there is at least one 
pair of consecutive time periods between which the treatment of some units, the switchers, changes, while the treatment of some
other units, the stayers, does not change. 
{p_end}

{p 4 4}
{cmd:Target parameters.}
The command can estimate the Average Of Switchers' Slopes (AOSS) and the Weighted Average Of Switchers' Slopes (WAOSS) parameters
introduced in {browse "https://ssrn.com/abstract=4011782":de Chaisemartin et al (2022)}.
The AOSS is the average, across switchers, of (Y_t(D_t)-Y_t(D_{t-1})/(D_t-D_{t-1}), the effect 
on their period-t outcome of moving their period-t
treatment from its period-(t-1) to its period-t value, scaled by the difference between these two values. The WAOSS is a weighted 
average of switchers' slopes (Y_t(D_t)-Y_t(D_{t-1})/(D_t-D_{t-1}), where slopes receive a weight proportional to |D_t-D_{t-1}|,
switchers’ absolute treatment change from period-(t-1) to period-t. The variance of the WAOSS estimator is often smaller 
than that of the AOSS estimator, especially when there are switchers that experience a small treatment change. 
The WAOSS estimator is also amenable to doubly-robust estimation, unlike the AOSS estimator.
{p_end}

{p 4 4}
{cmd:Assumptions.}
When the data has more than two time periods, the command assumes a static model: units' outcome at period t only depends on their period-t
treatment, not on their lagged treatments. See the {cmd:did_multiplegt_dyn} command for estimators allowing for dynamic effects. 
The command also makes a parallel trends assumption: the counterfactual outcome evolution
switchers would have experienced if their treatment had not changed is assumed to be equal to
the outcome evolution of stayers with the same baseline treatment. To test that assumption, the command can compute placebo estimators comparing
the outcome evolution of switchers and stayers with the same baseline treatment before switchers' treatment changes. 
{p_end}

{p 4 4}
{cmd:Estimators, when the exact_match option is specified.}
With a binary or discrete treatment, if the {cmd:exact_match} option is specified, the estimators computed by the command compare the 
outcome evolution of switchers and stayers 
with the same period-(t-1) treatment. Then, the WAOSS estimator computed by {cmd:did_multiplegt_stat} 
is numerically equivalent to the DID_M estimator proposed by 
{browse "https://aeaweb.org/articles?id=10.1257/aer.20181169":de Chaisemartin and D'Haultfoeuille (2020a)}, 
and computed by the {cmd:did_multiplegt} command. {cmd:did_multiplegt_stat} uses an analytic formula
to compute the estimator's variance, while {cmd:did_multiplegt} uses the bootstrap. 
Thus, the run time of {cmd:did_multiplegt_stat} is typically much lower. 
The {cmd:exact_match} option can only be specified when the treatment is binary or discrete:
with a continuously distributed treatment, one cannot find switchers and stayers 
with the exact same period-(t-1) treatment. 
With a discrete treatment taking a large number of values, specifying this option may be undesirable: 
then, there may only be few switchers that can be
matched to a stayer with the exact same period-(t-1) treatment, thus restricting the estimation sample.
{p_end}

{p 4 4}
{cmd:Estimators, when the exact_match option is not specified.}
When the {cmd:exact_match} option is not specified, the command can use a regression adjustment to recover switchers' 
counterfactual outcome evolution: for all t, it runs an OLS regression of Y_t-Y_{t-1} on a polynomial in D_{t-1} in the sample of (t-1)-to-t stayers, 
and uses that regression
to predict switchers' counterfactual outcome evolution. Alternatively, when it estimates the WAOSS, the command can also use propensity-score 
reweighting to recover switchers' counterfactual outcome evolution. First, for all t it estimates a logistic regression of an indicator 
for (t-1)-to-t switchers on a polynomial in D_{t-1}, to predict units' probability of being a switcher. 
Then, it computes a weighted average of stayers' outcome evolution, upweighting stayers with a large probability of being switchers, 
and downweighting stayers with a low probability of being switchers. Finally, when it estimates the WAOSS, the command can also combine 
regression-adjustment and propensity-score reweighting, thus yielding a doubly-robust estimator.
{p_end}

{p 4 4}
{cmd:Instrumental-variable case.}
There may be instances where the parallel-trends assumption fails, but one
has at hand an instrument satisfying a similar parallel-trends assumption. For instance, one may
be interested in estimating the price-elasticity of a good's consumption, but prices respond to
supply and demand shocks, and the counterfactual consumption evolution of units experiencing and not experiencing a price 
change may therefore not be the same. On the other hand, taxes
may not respond to supply and demand shocks and may satisfy a parallel-trends assumption. In such cases, the command
can compute the IV-WAOSS estimator introduced in 
{browse "https://ssrn.com/abstract=4011782":de Chaisemartin et al (2022)}.
The IV-WAOSS estimator is equal to the WAOSS estimator of the instrument's reduced-form effect on the outcome, divided by the
WAOSS estimator of the instrument's first-stage effect on the treatment.
{p_end}

{marker options}{...}
{title:Options}

{p 4 4}
[{opt bys:ort} {varlist}{cmd::}] makes {cmd:did_multiplegt_stat} byable. See {manhelp by D}. Only time-invariant variables are allowed in varlist.
{p_end}

{p 4 4}
{cmd:estimator(}{it:string}{cmd:)} gives the name(s) of the estimator(s) to be estimated. The allowed arguments are: (1) {cmd:aoss}, (2) {cmd:waoss}, 
and (3) {cmd:iv-waoss}. By default, the command computes the AOSS and WAOSS estimators if no instrumental variable
Z is specified, while it computes the IV-WAOSS if an instrumental variable
Z is specified.
{p_end}

{p 4 4}
{cmd:aoss_vs_waoss}: shows a test that the AOSS and WAOSS are equal. This option can only be used when estimation of the AOSS and WAOSS is requested.
{p_end}

{p 4 4}
{cmd:exact_match}: with this option, the DID estimators computed by the command compare the outcome evolution of switchers and stayers 
with the same period-(t-1) treatment (or instrument) value. This option can only be used when the treatment (or instrument) is binary or discrete:
with a continuously distributed treatment (or instrument), one cannot find switchers and stayers 
with the exact same period-(t-1) treatment (or instrument). 
With a discrete treatment taking a large number of values, specifying this option may be undesirable: then, there may only be few switchers that can be
matched to a stayer with the exact same period-(t-1) treatment, thus restricting the estimation sample.
{p_end}

{p 4 4}
{cmd: estimation_method(}{it:string}{cmd:)}: when the {cmd:exact_match} option is not specified and estimation of the WAOSS or IV-WAOSS is requested, 
this option can be used to specify which estimation method to use when estimating the WAOSS or IV-WAOSS. 
The allowed arguments are: (1) {cmd:ra} (regression adjustment), (2) {cmd:ps} (propensity-based reweighting), and (3) {cmd:dr} (doubly-robust). 
By default, a doubly-robust estimator is used.
{p_end}

{p 4 4}
{cmd:{ul:or}der(}{it:#}{cmd:)}: when the exact_match option is not specified, 
this option specifies the polynomial order to be used in the OLS regressions of Y_t-Y_{t-1} on a polynomial in D_{t-1}
and/or in the logistic regressions of an indicator for (t-1)-to-t switchers on a polynomial in D_{t-1}. 
By default, a polynomial of order 1 is used.
{p_end}

{p 4 4}
{cmd:controls({varlist}{cmd:})} :  One can control for covariates upon a modification of the parallel trends assumption: 
the counterfactual outcome evolution switchers would have
    experienced if their treatment had not changed is assumed to be equal to the outcome evolution of stayers with the same baseline
    treatment, and with the same value of {varlist}{cmd:}. 
{p_end}

{p 4 4}
{cmd:{ul:noextra}polation}: when this option is specified, the command only keeps switchers whose period-(t-1) treatment (or instrument) 
is between the minimum and the maximum values of the period-(t-1) treatment (or instrument) of stayers.
{p_end}

{p 4 4}
{cmd: switchers(}{it:string}{cmd:)}: if the argument {cmd:up} is inputted, the command estimates the AOSS, WAOSS, or IV-WAOSS 
for switchers-up only, i.e for units whose treatment (or instrument) increases from period (t-1) to t. If the argument {cmd:down}
is inputted, the command estimates the AOSS, WAOSS, or IV-WAOSS for switchers-down only, i.e. for units whose treament (or instrument)  
decreases from period (t-1) to t. By default, the command estimates those parameters for all switchers.
{p_end}

{p 4 4}
{cmd:{ul:disag}gregate}: when this option is specified, the command shows the estimated AOSS, WAOSS, or IV-WAOSS effects for each 
pair of consecutive time periods, on top of the effects aggregated across all time periods. By default, the command only shows
effects aggregated across all time periods.
{p_end}

{p 4 4}
{cmd:other_treatments({varlist}{cmd:})} : This option allows controlling for other treatments (in {cmd:{varlist}{cmd:}} ) that may also change over the panel.
{p_end}

{p 4 4}
{cmd:cluster({varlist}{cmd:})} : This option allows clustering the s.es at the level of {cmd:{varlist}{cmd:}}.
{p_end}

{p 4 4}
{cmd:placebo}: when this option is specified, the command computes the 
placebo version of each estimator requested.
{p_end}

{p 4 4}
{cmd:by_fd(}{it:#}{cmd:)}: This option can be used if one wants to assess the heterogeneity of the effect according to the absolute value of the changes in the treatment. For example, if {cmd:by_fd(}{it:5}{cmd:)} is specified, the command will split the switchers into 5 groups delimited by the 4 quantiles of the distribution of |deltaD_t| (or |deltaZ_t), and computes the models of each sample.
{p_end}

{p 4 4}
{cmd:bys_baseline(}{it:#}{cmd:)}: This option is similar to the option {cmd:by_fd(}{it:#}{cmd:)}, except that it uses the distribution of the baseline treatment D_{t-1} (or instrument Z_{t-1}).
{p_end}

{p 4 4}
{cmd:bys_graph_off}: If the program is by'd (i.e. ran with [{opt bys:ort} {varlist}{cmd::}]), or used with the option {cmd:by_fd(}{it:#}{cmd:)} or {cmd:by_baseline(}{it:#}{cmd:)}, it automatically displays a graph of the aggregated results (coefficients and confident intervals) by level of {varlist}, or quantiles, respectively. If {cmd:bys_graph_off} is specified, the graph is not displayed.
{p_end}

{p 4 4}
{cmd:bootstrap({it:#})}: In the case of weeak IV problem, resulting in a small first-stage estimand, the analytical s.e of the IV-WAOSS can be very imprecise. 
In such a case, we propose to use bootstrap to retrive more precise CIs. This option takes as argument the number of replications and allows to do so, and is only allowed when the IV-WAOSS is requested.
{p_end}

{p 4 4}
{cmd:seed({it:#})}: This option is only needed when one is running bootstrap, and it allows to set the seed so as to ensure replicability of the results.
{p_end}

{marker Example}{...}
{title:Examples}

{p 4 4}
These examples follow Section 7 of {browse "https://ssrn.com/abstract=4011782":de Chaisemartin et al (2022)}. 
The dataset used is an excerpt of that of {browse "https://www.aeaweb.org/articles?id=10.1257/pol.6.4.302":Li et al (2014)},
which contains gasoline taxes, prices, and consumption for 48 US states, every year from 1966 to 2008. It can be downloaded by running:
{p_end}

{phang2}{stata ssc install did_multiplegt_stat}{p_end}
{phang2}{stata net get did_multiplegt_stat}{p_end}
{phang2}{stata use gazoline_did_multiplegt_stat.dta, clear}{p_end}

{title:Example 1: Estimating the effect of gasoline taxes (tau) on log gasoline consumption (lngpinc)}

{phang2}{stata did_multiplegt_stat lngpinc id year tau, or(2) aoss_vs_waoss placebo}{p_end}

{title:Example 2: Estimating the effect of gasoline taxes (tau) on log gasoline prices (lngca)}

{phang2}{stata did_multiplegt_stat lngca id year tau, or(1) estimation_method(ra) aoss_vs_waoss placebo}{p_end}

{title:Example 3: Estimating the price-elasticity of gasoline consumption, using taxes as an instrument}

{phang2}{stata did_multiplegt_stat lngca id year lngpinc tau, or(2)  estimator(iv-waoss) placebo}{p_end}

{title:References}

{p 4 4}
de Chaisemartin, C, D'Haultfoeuille, X, Pasquier, F, Vazquez‐Bare, G (2022)
{browse "https://ssrn.com/abstract=4011782":Difference-in-Differences for Continuous Treatments and Instruments with Stayers}.
{p_end}
{p 4 4}
de Chaisemartin, C, D'Haultfoeuille, X (2020a)
{browse "https://aeaweb.org/articles?id=10.1257/aer.20181169":Two-Way Fixed Effects Estimators with Heterogeneous Treatment Effects}.
{p_end}
{p 4 4}
de Chaisemartin, C, D'Haultfoeuille, X (2020b)
{browse "https://papers.ssrn.com/sol3/papers.cfm?abstract_id=3751060": Two-way fixed effects regressions with several treatments.}.
{p_end}
{p 4 4}
Li, S, Linn, J, Muehlegger, E (2014)
{browse "https://www.aeaweb.org/articles?id=10.1257/pol.6.4.302":Gasoline Taxes and Consumer Behavior}.
{p_end}

{title:Authors}

{p 4 4}
Clément de Chaisemartin, Economics Department, Sciences Po, France.
{p_end}
{p 4 4}
Diego Ciccia, Sciences Po, France.
{p_end}
{p 4 4}
Xavier D'Haultfoeuille, CREST-ENSAE, France.
{p_end}
{p 4 4}
Felix Knau, Sciences Po, France.
{p_end}
{p 4 4}
Felix Pasquier, CREST-ENSAE, France.
{p_end}
{p 4 4}
Doulo Sow, Sciences Po, France.
{p_end}
{p 4 4}
Gonzalo Vazquez-Bare, UCSB, USA.
{p_end}

{title:Contact}

{p 4 4}
{browse "mailto:chaisemartin.packages@gmail.com":chaisemartin.packages@gmail.com}

{title:Stored results}

{cmd:  Matrices}

{p2col 5 16 30 2: Effects' estimation}{p_end}

{synopt:{cmd:e(AOSS)}}   The results for AOSS.{p_end}
{synopt:{cmd:e(WAOSS)}}  The results for WAOSS.{p_end}
{synopt:{cmd:e(IWAOSS)}} The results for IVWAOSS.{p_end}

{p2col 5 16 30 2: Placebos' estimation}{p_end}

{synopt:{cmd:e(PlaceboAOSS)}}   The results for AOSS.{p_end}
{synopt:{cmd:e(PlaceboWAOSS)}}  The results for WAOSS.{p_end}
{synopt:{cmd:e(PlaceboIWAOSS)}} The results for IVWAOSS.{p_end}

{p2col 5 16 30 2: If the program is by'd, for each level ℓ of varlist:}{p_end}

{synopt:{cmd:e(AOSS_ℓ)}}   The results for AOSS.{p_end}
{synopt:{cmd:e(WAOSS_ℓ)}}  The results for WAOSS.{p_end}
{synopt:{cmd:e(IWAOSS_ℓ)}} The results for IVWAOSS.{p_end}

{synopt:{cmd:e(PlaceboAOSS_ℓ)}}   The results for AOSS.{p_end}
{synopt:{cmd:e(PlaceboWAOSS_ℓ)}}  The results for WAOSS.{p_end}
{synopt:{cmd:e(PlaceboIWAOSS_ℓ)}} The results for IVWAOSS.{p_end}

{p2col 5 16 30 2: If the program is by'd, and the option {cmd:by_fd(}{it:#}{cmd:)} or {cmd:by_baseline(}{it:#}{cmd:)} is specified, for each level ℓ of varlist, and for each level k of quantile:}{p_end}

{synopt:{cmd:e(AOSS_ℓ_k)}}   The results for AOSS.{p_end}
{synopt:{cmd:e(WAOSS_ℓ_k)}}  The results for WAOSS.{p_end}
{synopt:{cmd:e(IWAOSS_ℓ_k)}} The results for IVWAOSS.{p_end}

{synopt:{cmd:e(PlaceboAOSS_ℓ_k)}}   The results for AOSS.{p_end}
{synopt:{cmd:e(PlaceboWAOSS_ℓ_k)}}  The results for WAOSS.{p_end}
{synopt:{cmd:e(PlaceboIWAOSS_ℓ_k)}} The results for IVWAOSS.{p_end}


