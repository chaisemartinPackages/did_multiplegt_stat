{smcl}
{* *! version 1  2024-01-29}{...}
{viewerjumpto "Syntax" "did_continuous##syntax"}{...}
{viewerjumpto "Description" "did_continuous##description"}{...}
{viewerjumpto "Options" "did_continuous##options"}{...}
{viewerjumpto "Examples" "did_continuous##examples"}{...}
{viewerjumpto "Saved results" "did_continuous##saved_results"}{...}

{title:Title}

{p 4 4}
{cmd:did_continuous} {hline 2} Estimation of Difference-in-Difference (DID) Estimators
for Treatments [and Instruments] Continuously Distributed at Every Period with Stayers.
{p_end}

{marker syntax}{...}
{title:Syntax}

{p 4 4}
{cmd:did_continuous Y ID T D [Z]} {ifin}
[{cmd:,}
{cmd:estimator(}{it:string}{cmd:)}
{cmd: estimation_method(}{it:string}{cmd:)}
{cmd:{ul:or}der(}{it:#}{cmd:)}
{cmd: switchers(}{it:string}{cmd:)}
{cmd:placebo}
{cmd:{ul:noextra}polation}
{cmd:{ul:disag}gregate}
{cmd:aoss_vs_waoss}]
{p_end}

{synoptset 28 tabbed}{...}

{marker description}{...}
{title:Description}

{p 4 4}
{cmd:did_continuous} estimates difference-in-differences estimators for continuous treatments with heterogeneous
effects, assuming that between consecutive periods, the treatment of some units,
the switchers, changes, while the treatment of other units does not change.
It computes the three estimators (including an IV-related estimator) introduced
in {browse "https://ssrn.com/abstract=4011782":de Chaisemartin, C, D'Haultfoeuille, X, Pasquier, F, Vazquez‐Bare, G,(2022)}.
The estimators computed by the command assume static effects and rely on a parallel trends assumptions.
{p_end}

{p 4 4}
The command can be used with more than two periods. If the number of periods is greater than two,
the command estimates, for each pair of two successive periods, the requested DID estimators (aoss, waoss or iwaoss)
as well as their aggregated versions defined as a weighted average of the estimators of the different pairs of periods.
The command can also be used when the panel data is unbalaced or presents gaps.
{p_end}

{p 4 4}
The command also computes, when the number of periods is larger than two, the placebos versions of the different estimators 
for each two successive time periods, and the aggregated versions.
Thus, allowing to test for parallel trends assumptions under which the proposed estimators computed by {cmd:did_continuous} are unbiased.
{p_end}

{p 4 4}
This command can also be used when the treatment is discrete.
In particular, when the treatment is discrete and takes a large number of values and the number 
of periods is equal to two,
{cmd:did_continuous} can be used as an alternative to the {cmd:did_multiplegt_dyn} command, 
which may not be applicable in such a design since it requires finding switchers and 
controls with the same period-one treatment. When the number of periods is larger than two, the two 
commands estimate two different models (static effects for {cmd:did_continuous}, and 
dynamic effects for {cmd:did_multiplegt_dyn}).


{p 4 4}
{cmd:Y} is the outcome variable.
{p_end}

{p 4 4}
{cmd:ID} is the identifier of the unit of analysis.
{p_end}

{p 4 4}
{cmd:T} is the time period variable.
The command assumes that the time variable is evenly spaced
(e.g.: the panel is at the yearly level,
and no year is missing for all groups).
When it is not (e.g.: the panel is at the yearly level,
but three consecutive years are missing for all
groups), the command can still be used. For example, if the year n is missing, the command does not comuptes the DID
estimators of the pairs of years (n-1,n),(n,n+1), and (n-1,n+1). .
{p_end}

{p 4 4}
{cmd:D} is the treatment variable.
{p_end}

{p 4 4}
{cmd:Z} (optional) is the instrumental variable, and is only required when the IV-related estimator (the so-called iwaoss
in de Chaisemartin, C, D'Haultfoeuille, X, Pasquier, F, Vazquez‐Bare, G,(2022)) is requested.
{p_end}

{marker options}{...}
{title:Options}

{p 4 4}
{cmd:estimator(}{it:string}{cmd:)} gives the name(s) of the estimator(s) to be estimated. The allowed arguments are: (1) {cmd:aoss}, i.e 
the Average Of Switchers’ Slopes which is the average, across switchers, of the effect on their period-(t) outcome of moving their
treatment from its period-(t-1) to its period-(t) value, scaled by the difference between these two values.
(2) {cmd:waoss} which corresponds to a weighted version of {cmd:aoss} where slopes receive a weight proportional to switchers’ absolute 
treatment change from period-(t-1) to period-(t). (3) {cmd:iwaoss} which generalizes {cmd:waoss} to the instrumental-variable case, and is
equal to the reduced-form {cmd:waoss} effect of the instrument on the outcome, divided by the
first-stage {cmd:waoss} effect of the instrument on the treatment.
{p_end}
{p 4 4}
If {cmd:estimator(}{it:string}{cmd:)} is not specified: by default, the command estimates both {cmd:aoss} and {cmd:waoss} if the instrumental-variable
Z is not specified, or only {cmd:iwaoss} otherwise. 
{p_end}

{p 4 4}
{cmd: estimation_method(}{it:string}{cmd:)}: This option allows to specify which estimation
method to use when estimating the {cmd:waoss} or the {cmd:iwaoss}, as described in
(de Chaisemartin, C, D'Haultfoeuille, X, Pasquier, F, Vazquez‐Bare, G,(2022)).
It takes
as argument {cmd:ra} (regression adjustment-based approach), or {cmd:ps} (propensity-based approach), 
or {cmd:dr} (double robust-based approach).
{p_end}

{p 4 4}
{cmd:{ul:or}der(}{it:#}{cmd:)} takes as argument the order of the polynomial series used to 
estimate the counterfactual of the variation of the outcome from period-(t-1) to period-t 
for the switchers, namely  E(ΔY_t |D_{t-1}, S_t = 0) or  E(ΔY_t |Z_{t-1}, SI_t = 0). 
{p_end}

{p 4 4}
{cmd: switchers(}{it:string}{cmd:)}: The allowed inputs for this option are {cmd:up} and {cmd:down}.
If the argument {cmd:up} is specified, the command estimates the effects on switchers-up,
i.e, units whose treatments (or instruments) increase from period-(t-1) to period-t. If the argument {cmd:down}
is given, the command estimates the effects on switchers-down, i.e., units whose treaments (or instruments)  decrease from 
period-(t-1) to period-t.
{p_end}

{p 4 4}
{cmd:{ul:disag}gregate}: If this potion is specified, the command displays the estimands of the effects for each 
two consecutive periods as well as the aggregated estimands. Otherwise, the command only outputs
the aggregated results.
{p_end}

{p 4 4}
{cmd:placebo}: This option allows to estimate the placebos versions of the estimators requested
in the option {cmd:estimator}. If this option is combined with the option {cmd:disaggregate}, the 
command also displays the placebo version of each two consecutive time-periods.
{p_end}

{p 4 4}
{cmd:{ul:noextra}polation}: This option forces the command to use only switchers whose period-(t-1) treatments (or instruments) 
are between the minimum and the maximum values of the period-(t-1) treatments (or instruments) of the stayers. This a less restrictive
common support assumption.
{p_end}

{p 4 4}
{cmd:aoss_vs_waoss}: As highlighted in (de Chaisemartin, C, D'Haultfoeuille, X, Pasquier, F, Vazquez‐Bare, G,(2022)), the {cmd:aoss} and 
the {cmd:woass} are equal if and only if switchers’ slopes are uncorrelated with |ΔD_t|. When this option is specified, the command performs
and displays the test of the equality between the {cmd:aoss} and  the {cmd:woass}. Note that the use of this option requires specifying in 
the {cmd:estimator} option both {cmd:aoss} and {cmd:waoss}, i.e., {cmd:estimator(aoss waoss)}.
{p_end}
{marker Example}{...}

{title:EXAMPLES}
{p 4 4}
The data for this example can be downloaded by running:
{p_end}

{phang2}{stata ssc did_continuous}{p_end}
{phang2}{stata net get did_continuous}{p_end}
{phang2}{stata use gazoline.dta, clear}{p_end}
Create the treatment variable as total of taxes (state plus federal).
{phang2}{stata gen tau =  fgastax + sgastax}{p_end}

{title:Example 1: Estimating the effect of gasoline taxes on gasoline consumption and prices}

{phang2}{stata did_continuous lngpinc id year tau, or(2)  estimator(aoss waoss) estimation_method(dr) disag aoss_vs_waoss placebo noextra}{p_end}

{phang2}{stata did_continuous lngca id year tau, or(2)  estimator(aoss waoss) estimation_method(dr) disag aoss_vs_waoss placebo noextra}{p_end}


{title:Example 2: Estimating the price-elasticity of gasoline consumption, using taxes as an instrument}

{phang2}{stata did_continuous lngca id year lngpinc tau, or(2)  estimator(iwaoss) estimation_method(dr) disag placebo noextra}{p_end}

{marker FAQ}{...}
{title:FAQ}

{p 4 4}
{p_end}

{title:References}

{p 4 4}
de Chaisemartin, C, D'Haultfoeuille, X, Pasquier, F, Vazquez‐Bare, G,(2022).
{browse "https://ssrn.com/abstract=4011782":Difference-in-Differences for Continuous Treatments and Instruments with Stayers}.
{p_end}

{title:Auxiliary packages}


{title:Authors}


{title:Contact}

{marker saved_results}{...}
{title:Saved results}
