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
with a binary, discrete, or continuous treatment or instrument, in designs with stayers, assuming that past treatments
do not affect the current outcome.
{p_end}

{marker syntax}{...}
{title:Syntax}

{p 4 4}
[{opt bys:ort} {varlist}{cmd::}]
{cmd:did_multiplegt_stat Y G T D [Z]} {ifin}
[{cmd:,}
{cmd:estimator(}{it:string}{cmd:)}
{cmd:exact_match}
{cmd:as_vs_was}
{cmd:{ul:or}der(}{it:#/####/########}{cmd:)}
{cmd:cross_fitting({it:#})}
{cmd:controls({varlist}{cmd:})}
{cmd:weights({varname}{cmd:})}
{cmd:cluster({varlist}{cmd:})}
{cmd: switchers(}{it:string}{cmd:)}
{cmd:placebo(}{it:#}{cmd:)}
{cmd:on_placebo_sample}
{cmd:twfe(}{help did_multiplegt_stat##twfe_suboptions:twfe_suboptions}{cmd:)]} 
{cmd:{ul:noextra}polation}
{cmd:trimming(#)}
{cmd:other_treatments({varlist}{cmd:})}
{cmd:by_fd(}{it:#}{cmd:)}
{cmd:by_baseline(}{it:#}{cmd:)}
{cmd:{ul:disag}gregate}
{cmd:graph_off}
{cmd:bys_graph_off}
{cmd:bootstrap({it:#})}
{cmd:seed({it:#})}
{p_end}

{p 4 4}
{cmd:Y} is the outcome variable.
{p_end}

{p 4 4}
{cmd:G} is the identifier of the unit of analysis.
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
{cmd:Data and design.} The command uses panel data at the {cmd:(G,T)} level to estimate heterogeneity-robust DID estimators, 
with a binary, discrete, or continuous treatment (or instrument). The command can be used in designs where there is at least one 
pair of consecutive time periods between which the treatment of some units, the switchers, changes, while the treatment of some
other units, the stayers, does not change. 
{p_end}

{p 4 4}
{cmd:Target parameters.}
The command can estimate the Average Slope (AS) and the Weighted Average Slope (WAS) parameters
introduced in {browse "https://ssrn.com/abstract=4011782":de Chaisemartin et al (2025)}.
The AS is the average, across switchers, of (Y_t(D_t)-Y_t(D_{t-1})/(D_t-D_{t-1}), the effect 
on their period-t outcome of moving their period-t
treatment from its period-(t-1) to its period-t value, scaled by the difference between these two values. The WAS is a weighted 
average of switchers' slopes (Y_t(D_t)-Y_t(D_{t-1})/(D_t-D_{t-1}), where slopes receive a weight proportional to |D_t-D_{t-1}|,
switchers' absolute treatment change from period-(t-1) to period-t. The variance of the WAS estimator is often smaller 
than that of the AS estimator, especially when there are switchers that experience a small treatment change. 
{p_end}

{p 4 4}
{cmd:Assumptions.}
When the data has more than two time periods, the command assumes a static model: units' outcome at period t only depends on their period-t
treatment, not on their lagged treatments. See the {cmd:did_multiplegt_dyn} command for estimators allowing for dynamic effects. 
The command also makes a parallel trends assumption: the counterfactual outcome evolution
switchers would have experienced if their treatment had not changed is assumed to be equal to
the outcome evolution of stayers with the same baseline treatment. Importantly, this parallel-trends 
assumption is conditional on the baseline treatment: comparing switchers and stayers
with different baseline treatments would implicitly amount to assuming that the treatment's
effect is constant over time.
To test the parallel trends assumption underlying the estimators, 
the command can compute placebo estimators comparing
the outcome evolution of switchers and stayers with the same baseline treatment before switchers' treatment changes. 
{p_end}

{p 4 4}
{cmd:Estimators, when the exact_match option is specified.}
With a binary or discrete treatment, if the {cmd:exact_match} option is specified, the estimators computed by the command compare the 
outcome evolution of switchers and stayers 
with the same period-(t-1) treatment. Then, the WAS estimator computed by {cmd:did_multiplegt_stat} 
is numerically equivalent to the DID_M estimator proposed by 
{browse "https://aeaweb.org/articles?id=10.1257/aer.20181169":de Chaisemartin and D'Haultfoeuille (2020)}, 
and already computed by the {cmd:did_multiplegt_old} command. {cmd:did_multiplegt_stat} uses an analytic formula
to compute the estimator's variance, while {cmd:did_multiplegt_old} uses the bootstrap. 
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
When the {cmd:exact_match} option is not specified, the command computes a doubly-robust estimator, that combines regression
 adjustment and propensity-score reweighting to compare switchers and stayers controlling for their  period-(t-1) treatment.
  The regression adjustment amounts to regressing, for all t, Y_t-Y_{t-1} on a polynomial in D_{t-1} in the sample of
   (t-1)-to-t stayers, and use the regression to predict switchers' Y_t-Y_{t-1}. Propensity score reweighting is based on
    logistic regressions of an indicator for (t-1)-to-t switchers on a polynomial in D_{t-1}.
{p_end}

{p 4 4}
{cmd:Instrumental-variable case.}
There may be instances where the parallel-trends assumption fails, but one
has at hand an instrument satisfying a similar parallel-trends assumption. For instance, one may
be interested in estimating the price-elasticity of a good's consumption, but prices respond to
supply and demand shocks, and the counterfactual consumption evolution of units experiencing and not experiencing a price 
change may therefore not be the same. On the other hand, taxes
may not respond to supply and demand shocks and may satisfy a parallel-trends assumption. In such cases, the command
can compute the IV-WAS estimator introduced in 
{browse "https://ssrn.com/abstract=4011782":de Chaisemartin et al (2025)} using doubly-robust estimators.
The IV-WAS estimator is equal to the WAS estimator of the instrument's reduced-form effect on the outcome controlling for D_{t-1}, divided by the
WAS estimator of the instrument's first-stage effect on the treatment controlling for D_{t-1}. 
See {browse "https://ssrn.com/abstract=4011782":de Chaisemartin et al (2025)} for some explanations as to why controlling for D_{t-1}
is desirable in IV estimation.
{p_end}

{marker options}{...}
{title:Options}

{dlgtab:Main options}

{phang}
{cmd:estimator(}{it:string}{cmd:)} gives the name(s) of the estimator(s) to be estimated. The allowed arguments are: (1) {cmd:as}, (2) {cmd:was}, 
and (3) {cmd:iv-was}. If multiple estimators are requested, e.g., {cmd:as} and {cmd:was}, they must be separated by a single space. 

{phang}
{cmd:exact_match}: with this option, the DID estimators computed by the command compare the outcome evolution of switchers and stayers 
with the same period-(t-1) treatment (or instrument) value. This option can only be used when the treatment (or instrument) is binary or discrete:
with a continuously distributed treatment (or instrument), one cannot find switchers and stayers 
with the exact same period-(t-1) treatment (or instrument). 
With a discrete treatment taking a large number of values, specifying this option may be undesirable: then, there may only be few switchers that can be
matched to a stayer with the exact same period-(t-1) treatment, thus restricting the estimation sample.

{phang}
{cmd:{ul:or}der(}{it:#/####/########}{cmd:)}: when the exact_match option is not specified, 
this option specifies the polynomial orders to be used in the OLS regressions of Y_t-Y_{t-1} on a polynomial in D_{t-1}
and/or in the logistic regressions of an indicator for (t-1)-to-t switchers on a polynomial in D_{t-1}. 
This option allows for either 1, 4, or 8 arguments, with 8 arguments only allowed when IV-WAS requested. 
E.g.: order(1), order(1 4 3 2) or order(1 4 3 2 1 2 3 4) are allowed (the last one only if IV-WAS specified), but order(1 2 3) is not allowed. 
With 4 arguments, argument 1, 2, 3 and 4, is the order used to estimate E(Y_t-Y_{t-1}|D_{t-1}), P(S_{t}=0|D_{t-1}), P(S_{+, t}=1|D_{t-1}), and P(S_{-, t}=1|D_{t-1}), respectively. 
With 8 arguments the same logic is applied, but the first 4 arguments are for the first stage, and the next 4 for the reduced form. 
If IV-WAS is requested but order has 4 arguments, we apply the same orders to first stage and reduced form. If order has only one argument, the same order is used for all estimations.
By default, a polynomial of order 1 is used.

{phang}
{cmd:placebo(}{it:#}{cmd:)}: when this option is specified, the command computes the 
placebo version of each estimator requested. Actual estimators compare the
t-1-to-t outcome evolution of period t-1-to-t switchers and stayers with the same baseline
    treatment. When {it:#} is equal to 1, placebo estimators compare the
t-2-to-t-1 outcome evolution of period t-1-to-t switchers and stayers with the same baseline
    treatment, restricting attention to t-2-to-t-1 stayers. Thus, placebos assess whether switchers and stayers were on parallel trends just before
switchers switched treatment. When {it:#} is strictly larger than 1, placebos comparing the outcome evolutions of  t-1-to-t switchers and stayers from
t-3 to t-2, from t-4 to t-3,... , and from t-{it:#}-1 to t-{it:#} are also reported, always restricting attention to stayers between those pairs
of periods.

{phang}
{cmd:as_vs_was}: shows a test that the AS and WAS are equal. This option can only be used when estimation of both AS and WAS is requested, using {cmd:estimator(as was)}.

{phang}
{cmd:controls({varlist}{cmd:})}: the command can compute estimators with control variables. They rely on a conditional parallel trends assumption:
the counterfactual outcome evolution switchers would have experienced if their treatment had not changed is assumed to be equal 
to the outcome evolution of stayers with the same baseline
    treatment, and with the same value of {varlist}{cmd:}. When time-varying control variables are inputted to the command, the command compares the
t-1-to-t outcome evolution of switchers and stayers with the same baseline
    treatment, and with the same controls at period t-1.
Specifying too many control variables may lead to noisy estimators. If placebo estimators are small, insignificant and precisely estimated without
control variables, including control variables may not be necessary.

{phang}
{cmd:weights({varname}{cmd:})} : This option allows to compute estimators weighted by {varname}{cmd:}.

{phang}
{cmd:trimming(#)} : This option takes as argument an integer between 0 and 100. If the as or was estimator is requested and # is
 the option's argument, the command trims observations for which the estimated probability P(S_{t}=0|D_{t-1}) is lower than #.
  If the iv-was estimator is requested, the command trims observations such that P(SI_{t}=1|Z_{t-1}, D_{t-1}) is lower than #.
   This option is only relevant when the probabilities are estimated using logit models, thus the option is not compatible with
    the option {cmd:exact_match}. 

{dlgtab:Options to estimate heterogeneous treatment effects}

{phang}
{cmd: switchers(}{it:string}{cmd:)}: if the argument {cmd:up} is inputted, the command estimates the AS, WAS, or IV-WAS 
for switchers-up only, i.e for units whose treatment (or instrument) increases from period (t-1) to t. If the argument {cmd:down}
is inputted, the command estimates the AS, WAS, or IV-WAS for switchers-down only, i.e. for units whose treament (or instrument)  
decreases from period (t-1) to t. By default, the command estimates those parameters for all switchers.

{phang}
{cmd:by_fd(}{it:#}{cmd:)}: This option can be used if one wants to assess the heterogeneity of the effect 
according to the absolute value of the treatment's (or instrument's) change. 
For example, if {cmd:by_fd(}{it:5}{cmd:)} is specified, the command 
splits switchers into 5 groups: the 20% with the lowest |Delta D_t| (or |Delta Z_t|), and then the next 20%, etc..  
Then the command estimates treatment effects for each subsample. If |Delta D_t| has mass points, the command splits switchers into
groups with as-equal-as-possible sizes. 

{phang}
{cmd:by_baseline(}{it:#}{cmd:)}: This option is similar to the option {cmd:by_fd(}{it:#}{cmd:)}, except that switchers
are split into subsamples according to their values of D_{t-1} (or Z_{t-1}).

{phang}
[{opt bys:ort} {varlist}{cmd::}] makes {cmd:did_multiplegt_stat} byable. See {manhelp by D}. Only time-invariant variables are allowed in varlist.

{dlgtab:Standard-error options}

{phang}
{cmd:bootstrap({it:#})}: If the number of switchers or the number of stayers is low, 
one may want to check if the analytic standard errors produced by the command are close to 
bootstrap standard errors. If they are not, this may indicate that the asymptotic approximation underlying the analytic standard errors may not be reliable.
In that case, there is of course no guarantee that bootstrap standard errors are valid: this comparison is just a diagnostic check
researchers may use to assess if they need to resort to inference methods that do not rely on asymptotic approximations, like permutation tests.
The {cmd:bootstrap} option takes as argument the number of replications. Currently, it is only allowed when the IV-WAS is requested,
as failure of asymptotic approximations are more likely to arise with IV estimators, when the first-stage is weak.

{phang}
{cmd:seed({it:#})}: This option is only needed when one is using the bootstrap, and it allows to set the seed so as to ensure replicability of the results.

{phang}
{cmd:cluster({varlist}{cmd:})} : This option allows clustering standard errors at the level of {cmd:{varlist}{cmd:}}.

{dlgtab:Advanced options}

{phang}
{cmd:other_treatments({varlist}{cmd:})} : This option allows controlling for other treatments (in {cmd:{varlist}{cmd:}} ) 
that may also change over the panel, see {browse "https://papers.ssrn.com/sol3/papers.cfm?abstract_id=3751060":de Chaisemartin and D'Haultfoeuille (2021)} 
for further details.

{phang}
{cmd:{ul:noextra}polation}: when this option is specified, the command only keeps switchers whose period-(t-1) treatment (or instrument) 
is between the minimum and the maximum values of the period-(t-1) treatment (or instrument) of stayers, thus enforcing the overlap condition.

{phang}
{cmd:cross_fitting(#)}: when this option is specified, the command performs a cross-fitting (with # splits) to estimate the requested
 doubly-robust estimator. For instance, if you specify {cmd:cross_fitting(2)}, the command splits randomly the sample into two subsamples
  (I_1 and I_2) and proceeds as follows. It uses subsample I_1 to estimate the nuisance functions, and estimates the parameter of interest
   using only subsample I_2. Then, the command redoes the same procedure, reverting the role of the two subsamples. 
   The final point estimate is then a weighted average of the two point estimates. See Section 3.3 
   of {browse "https://ssrn.com/abstract=4011782":de Chaisemartin et al (2025)} for details.

{phang}
{cmd:on_placebo_sample}: This option allows to compute the treatment-effect estimator on the subsample where the first placebo estimator
 is computed. The resulting treatment-effect estimator remains valid if the first treatment lag affects the outcome.

{marker twfe_suboptions}{...}
{dlgtab:TWFE Comparison}

{synoptset 22}{...}
{phang} The command allows to compare the estimator specified in {cmd:estimator()} to a TWFE-estimator, 
computed via a regression of Y_{i,t} on D_{i,t} and unit and year fixed effects. 
If the iv-was is specified in {cmd:estimator()}, a 2SLS-TWFE is used, using Z_{i,t} as the instrument. 
With the option {cmd: twfe}, {cmd:did_multiplegt_stat}, on top of the main results, 
diplays a table showing the difference between the estimator requested and the TWFE-estimator, the p-value of the test of the difference and the 
corresponding condidence interval.
By default, the command runs the test using 100 bootstrap replications. 
To increase the number of replications, the user can use the option {cmd: bootstrap(#)} of {cmd: did_multiplegt_stat}.
Also for replicability you should consider using the {cmd:seed(#)} option. 

{phang} To use the {cmd:twfe(}{help did_multiplegt_stat##twfe_suboptions:twfe_suboptions}{cmd:)} option you need to specify 
which sample you want to estimate the TWFE regression on, so you should always specify 
either the {cmd:full_sample} or the {cmd:same_sample} suboption.

{synopthdr:twfe_suboptions}
{synoptline}

{synopt:{cmd:same_sample}} Sometimes, {cmd: did_multiplegt_stat} might not use all time periods in the estimation. 
For instance, one might have a panel data where at a particular time (say p) there
is no switcher. In that case, {cmd: did_multiplegt_stat} does not use the pair of periods (p-1, p). Then, the ({cmd: as, was, or iv-was})
estimator and the TWFE-estimator will rely on different samples. 
To avoid such discrepancy, the option {cmd: same_sample} allows to estimate the TWFE-estimator using the same sample
as {cmd:did_multiplegt_stat}.{p_end} 

{synopt:{cmd:full_sample}} Counterpart to the {cmd:same_sample} option. Use this in case you do not want to impose the sample restrictions
described in {cmd:same_sample} and estimate the TWFE regression in the full sample instead.{p_end} 

{synopt:{cmd:percentile}} By default, {cmd: did_multiplegt_stat} computes the s.e and p-value of the test of the difference 
between the ({cmd:as, was, or iv-was}) estimator and the TWFE-estimator
using a t-test and a normal approximation. Instead, one may use the percentile boostrap. Then, one can specify the option {cmd: percentile} 
to compute p-values and confidence intervals using the percentile bootstrap method.{p_end}

{dlgtab:Display}

{phang}
{cmd:{ul:disag}gregate}: when this option is specified, the command shows the estimated AS, WAS, or IV-WAS effects for each 
pair of consecutive time periods, on top of the effects aggregated across all time periods. By default, the command only shows
effects aggregated across all time periods.

{phang}
{cmd:graph_off}: The program displays by default a graph of the aggregated 
results (coefficients, effects and placebos, and their confidence intervals). If {cmd:graph_off} is specified, 
the graph is not displayed.

{phang}
{cmd:bys_graph_off}: If the program is by'd (i.e. ran with [{opt bys:ort} {varlist}{cmd::}]), or used with the option {cmd:by_fd(}{it:#}{cmd:)} 
or {cmd:by_baseline(}{it:#}{cmd:)}, it automatically displays a graph of the aggregated 
results (coefficients and confidence intervals) by level of {varlist}, or quantiles, respectively. If {cmd:bys_graph_off} is specified, 
the graph is not displayed.

{marker Notes}{...}
{title:Notes}
{p 4 4}
1. The command is compatible with {cmd:{help estout}{cmd:}}{p_end}

{p 4 4}
2. The command is byable. See {manhelp by D}.
{p_end}

{marker Example}{...}
{title:Examples}

{p 4 4}
These examples follow Section 7 of {browse "https://ssrn.com/abstract=4011782":de Chaisemartin et al (2024)}. 
The dataset used is an excerpt of that of {browse "https://www.aeaweb.org/articles?id=10.1257/pol.6.4.302":Li et al (2014)},
which contains gasoline taxes, prices, and consumption for 48 US states, every year from 1966 to 2008. It can be downloaded by running:
{p_end}

{phang2}{stata ssc install did_multiplegt_stat}{p_end}
{phang2}{stata net get did_multiplegt_stat}{p_end}
{phang2}{stata use gazoline_did_multiplegt_stat.dta, clear}{p_end}

{title:Example 1: Estimating the effect of gasoline taxes (tau) on log gasoline price (lngpinc)}

{phang2}{stata did_multiplegt_stat lngpinc id year tau, or(1) estimator(as was)  placebo(3)  as_vs_was}{p_end}

{title:Example 2: Estimating the effect of gasoline taxes (tau) on log gasoline consumption (lngca)}

{phang2}{stata did_multiplegt_stat lngca id year tau, or(1) estimator(as was)  placebo(3)  as_vs_was}{p_end}

{title:Example 3: IV estimate of the price-elasticity of gasoline consumption}

{phang2}{stata did_multiplegt_stat lngca id year lngpinc tau, or(1) estimator(iv-was)  placebo(3) }{p_end}

{title:References}

{p 4 4}
de Chaisemartin, C, D'Haultfoeuille, X, Pasquier, F, Sow, D, Vazquez‐Bare, G (2024)
{browse "https://ssrn.com/abstract=4011782":Difference-in-Differences for Continuous Treatments and Instruments with Stayers}.
{p_end}
{p 4 4}
de Chaisemartin, C, D'Haultfoeuille, X (2020)
{browse "https://aeaweb.org/articles?id=10.1257/aer.20181169":Two-Way Fixed Effects Estimators with Heterogeneous Treatment Effects}.
{p_end}
{p 4 4}
de Chaisemartin, C, D'Haultfoeuille, X (2021)
{browse "https://papers.ssrn.com/sol3/papers.cfm?abstract_id=3751060": Two-way Fixed Effects and Differences-in-Differences Estimators with Several Treatments}.
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

{cmd:  Scalars}
{synoptline}

{synopt:{cmd:e(N) }}   The number of observations.{p_end}

{cmd:  Macros}
{synoptline}

{synopt:{cmd:e(depvar) }}   The dependent variable's name.{p_end}

{cmd:  Matrices}
{synoptline}

{p2col 5 16 30 2: Aggregated point estimates}{p_end}
{synopt:{cmd:e(b)}}   The point estimates of the aggregated versions of the estimators.{p_end}
{synopt:{cmd:e(V)}}   The variance matrix.{p_end}


{p2col 5 16 30 2: Effects' estimation}{p_end}

{synopt:{cmd:e(AS)}}   The results for AS.{p_end}
{synopt:{cmd:e(WAS)}}  The results for WAS.{p_end}
{synopt:{cmd:e(IWAS)}} The results for IVWAS.{p_end}

{p2col 5 16 30 2: Placebos' estimation}{p_end}

{synopt:{cmd:e(Placebo_p_AS)}}   The results for the Placebo(p) version of AS.{p_end}
{synopt:{cmd:e(Placebo_p_WAS)}}  The results for the Placebo(p) version of WAS.{p_end}
{synopt:{cmd:e(PlacebopIWAS)}} The results for the Placebo(p) version of IVWAS.{p_end}

{p2col 5 16 30 2: If the program is by'd, for each level ℓ of varlist:}{p_end}

{synopt:{cmd:e(AS_ℓ)}}   The results for AS.{p_end}
{synopt:{cmd:e(WAS_ℓ)}}  The results for WAS.{p_end}
{synopt:{cmd:e(IWAS_ℓ)}} The results for IVWAS.{p_end}

{synopt:{cmd:e(Placebo_p_AS_ℓ)}}   The results for the Placebo(p) version of AS.{p_end}
{synopt:{cmd:e(Placebo_p_WAS_ℓ)}}  The results for the Placebo(p) version of WAS.{p_end}
{synopt:{cmd:e(Placebo_p_IWAS_ℓ)}} The results for the Placebo(p) version of IVWAS.{p_end}

{p2col 5 16 30 2: If the program is by'd, and the option {cmd:by_fd(}{it:#}{cmd:)} or {cmd:by_baseline(}{it:#}{cmd:)}
 is specified, for each level ℓ of varlist, and for each level k of quantile:}{p_end}

{synopt:{cmd:e(AS_ℓ_k)}}   The results for AS.{p_end}
{synopt:{cmd:e(WAS_ℓ_k)}}  The results for WAS.{p_end}
{synopt:{cmd:e(IWAS_ℓ_k)}} The results for IVWAS.{p_end}

{synopt:{cmd:e(Placebo_p_AS_ℓ_k)}}   The results for the Placebo(p) version of AS.{p_end}
{synopt:{cmd:e(Placebo_p_WAS_ℓ_k)}}  The results for the Placebo(p) version of WAS.{p_end}
{synopt:{cmd:e(Placebo_p_IWAS_ℓ_k)}} The results for the Placebo(p) version of IVWAS.{p_end}


