/***********************************************************************
	  I.  GAZOLINE DATA TESTING DIFFERENT OPTIONS
***********************************************************************/
qui do "Stata/did_multiplegt_stat.ado"
use "gazoline_did_multiplegt_stat.dta", clear

//Reproducing first-stage and reduced-form from paper
did_multiplegt_stat lngca id year tau, or(1)  controls(lngpinc) estimator(aoss waoss)  placebo(3)  aoss_vs_waoss
did_multiplegt_stat lngca id year tau, or(2)  controls(lngpinc) estimator(aoss waoss)  placebo(3)  aoss_vs_waoss
did_multiplegt_stat lngpinc id year tau, or(1)  controls(lngpinc) estimator(aoss waoss)  placebo(3)  aoss_vs_waoss
did_multiplegt_stat lngpinc id year tau, or(2)  controls(lngpinc) estimator(aoss waoss)  placebo(3)  aoss_vs_waoss

// Reproducing iv from paper
did_multiplegt_stat lngca id year lngpinc tau, controls(lngpinc) or(1) estimator(iv-waoss) estimation_method(dr) 
did_multiplegt_stat lngca id year lngpinc tau, controls(lngpinc) or(2) estimator(iv-waoss) estimation_method(dr)

// CC: we need to fix graph issue when only one estimator is requested.
// CC: for later: we do not normalize graph at 0: effect shown at 0, as in did_multiplegt, and we keep the same legend as did_multiplegt "Time relative to treatment change (t=0)".
// Diego: solved issue 1.
did_multiplegt_stat lngpinc id year tau, or(2)  controls(lngpinc) estimator(waoss) placebo(3)

// GVB: we should get an error message when we specify negative polynomial order or a 0 polynomial order: polynomial order should be >=1
did_multiplegt_stat lngpinc id year tau, or(-1)  controls(lngpinc) estimator(waoss)  placebo(3)
did_multiplegt_stat lngpinc id year tau, or(0)  controls(lngpinc) estimator(waoss)  placebo(3)
// Diego: solved.

//Bootstrap + iv-waoss
did_multiplegt_stat lngca id year lngpinc tau, or(1)  estimator(iv-waoss) estimation_method(dr) bootstrap(2)
did_multiplegt_stat lngca id year lngpinc tau, or(1)  estimator(iv-waoss) estimation_method(dr) bootstrap(2) seed(1)
did_multiplegt_stat lngca id year lngpinc tau, or(1)  estimator(iv-waoss) estimation_method(dr) bootstrap(2) seed(1)

//weights
did_multiplegt_stat lngca id year tau, or(1) weight(lngpinc) estimator(waoss) estimation_method(dr) placebo(2)
did_multiplegt_stat lngca id year tau, or(1) estimator(waoss) estimation_method(dr) placebo(2)

// more than one control variable
did_multiplegt_stat lngca id year tau, or(1)  controls(lngpinc lncars) estimator(aoss waoss)  placebo(3)  aoss_vs_waoss
did_multiplegt_stat lngca id year tau, or(2)  controls(lngpinc lncars) estimator(aoss waoss)  placebo(3)  aoss_vs_waoss

// estimation methods
did_multiplegt_stat lngca id year tau, or(1) estimator(waoss) estimation_method(ps) placebo(2)
did_multiplegt_stat lngca id year tau, or(1) estimator(waoss) estimation_method(ra) placebo(2)
did_multiplegt_stat lngca id year tau, or(1) estimator(aoss) estimation_method(ps) placebo(2)

// up/down
did_multiplegt_stat lngca id year tau, or(1) estimator(waoss) switchers(up)
did_multiplegt_stat lngca id year tau, or(1) estimator(waoss) switchers(down)

// bysort:
// CC: bysort does not do anything, needs to be fixed.
gen Democrat_Gov1966_temp=Democrat_Gov if year==1966
bys state: egen Democrat_Gov1966=sum(Democrat_Gov1966_temp)
bysort Democrat_Gov1966: did_multiplegt_stat lngca id year tau, or(1) estimator(waoss) graph_off
did_multiplegt_stat lngca id year tau, or(1) estimator(waoss)
bysort Democrat_Gov1966: did_multiplegt_stat lngca id year tau, or(1) estimator(waoss)
// Diego: solved.

//No extrapolation
did_multiplegt_stat lngca id year tau, or(1) estimator(waoss) switchers(up) noextra
did_multiplegt_stat lngca id year tau, or(1) estimator(waoss) switchers(up) 

//cluster 
gen cluster_id_letter=substr(state,1,1) 
did_multiplegt_stat lngca id year tau, or(1) controls(lngpinc) estimator(waoss) estimation_method(dr) placebo(1) cluster(cluster_id_letter)
did_multiplegt_stat lngca id year tau, or(1) controls(lngpinc) estimator(waoss) estimation_method(dr) placebo(1)

// DC spotted an instance where, with clustering, standard error of point estimate changes with and without a placebo, but this is not the case here.
did_multiplegt_stat lngca id year tau, or(1) controls(lngpinc) estimator(waoss) estimation_method(dr) cluster(cluster_id_letter)

//twfe vs waoss + percentile bootstrap + same_sample, and waoss vs aoss
// CC: for later: maybe better to report the TWFE estimator in fixed sample, rather than average of TWFEs across bootstrap samples.
// CC: for later: perhaps make it possible to specify twfe without any option.

did_multiplegt_stat lngca id year tau, or(1)  estimator(aoss waoss) estimation_method(dr)  bootstrap(5) placebo(1) twfe(percentile same_sample) aoss_vs_waoss

//twfe vs waoss + normal bootstrap + same_sample, and waoss vs aoss
did_multiplegt_stat lngca id year tau, or(1)  estimator(aoss waoss) estimation_method(dr)  bootstrap(5) placebo(1) twfe(same_sample) aoss_vs_waoss

//2sls-twfe vs iv-waoss
did_multiplegt_stat lngca id year lngpinc tau , or(1)  estimator(iv-waoss) estimation_method(dr)  bootstrap(5) placebo(1) twfe(percentile same_sample)

//Cross_validation
// GVB: the cross-validation option should not give a warning message about the order when order is not specified:

did_multiplegt_stat lngca id year tau, estimator(aoss waoss) placebo(1) estimation_method(dr)  cross_validation(algo(loocv) tole(0.01) max_k(3) seed(1) kfolds(2)) 

// CC: the cross validation option does not seem compatible with the by_baseline option, we will fix this later, 
// for now I just indicated it in the help file, and we just need to shut down this combination of options and give an error message when specified together.
did_multiplegt_stat lngca id year tau, or(1) estimator(aoss waoss) placebo(1) estimation_method(dr)  cross_validation(algo(loocv) tole(0.01) max_k(2) seed(1) kfolds(2)) by_baseline(5)
did_multiplegt_stat lngca id year tau, or(1) estimator(aoss waoss) placebo(1) estimation_method(dr)  cross_validation(algo(loocv) tole(0.01) max_k(2) seed(1) kfolds(2)) by_fd(2)

/***********************************************************************
	   II. GAZOLINE DATA ESTOUT OPTION
***********************************************************************/
eststo clear

//Panel A: Reduced-form effect of taxes on quantities consumed.

global PanelA "posthead(\addlinespace \midrule[2pt] \multicolumn{3}{c}{\bf{Panel A: Reduced-form effect of taxes on quantities consumed}}\\\midrule[2pt] )"

global PanelAp "posthead(\addlinespace \midrule[2pt] \multicolumn{3}{c}{\bf{Panel A: Reduced-form placebo(1) effect of taxes on quantities consumed}}\\\midrule[2pt] )"
//1. Linear model
eststo linearReducedForm: did_multiplegt_stat lngca id year tau, or(1) estimation_method(dr) aoss_vs_waoss placebo(1) controls(lngpinc)

// CC: for later: the command seems to not produce a scalar pval_XX anymore, is that normal?
*estadd scalar pvalue = scalar(pval_XX)
estadd local  control "\checkmark", replace  

//2. Quadratic model
eststo quadReducedForm:  did_multiplegt_stat lngca id year tau, or(2) estimation_method(dr) aoss_vs_waoss placebo(1) controls(lngpinc)

// CC: for later: the command seems to not produce a scalar pval_XX anymore, is that normal?
*estadd scalar pvalue = scalar(pval_XX), replace
estadd local  control "\checkmark", replace 

//Effect Panel A
esttab linearReducedForm quadReducedForm  using "AOSS_WAOSS_Effects_controls.tex"  ,cells(b(fmt(%12.4fc)) se(fmt(%12.4fc) par) ci(fmt(%12.4fc) par)) stats(N pvalue control, fmt(%12.0fc %12.4fc 0 )  labels("Observations" "\bf{p.value}" "Controls") ) ///
mtitles("(1) Linear model" "(2) Quadratic model") nonumber collab(none) ///
/*addnote("Standard errors in parenthesis." "Confidence intervals in brackets.")*/ keep(AOSS WAOSS) label ///
${PanelA} replace

//Placebo Panel A
esttab linearReducedForm quadReducedForm  using "AOSS_WAOSS_Placebos_controls.tex"  ,cells(b(fmt(%12.4fc)) se(fmt(%12.4fc) par) ci(fmt(%12.4fc) par)) stats(N control, fmt(%12.0fc 0 )  labels("Observations" "Controls") ) ///
mtitles("(1) Linear model" "(2) Quadratic model") nonumber collab(none) ///
/*addnote("Standard errors in parenthesis." "Confidence intervals in brackets.")*/ keep(PlaceboAOSS PlaceboWAOSS) label ///
${PanelAp} append

/******************************************************************************
	   III. By Quantile and FD
******************************************************************************/

//1. The command

// CC: for later: graphs by fd get erased by ES graph when by_fd is specified. We do not do ES graph when by_fd or by_baseline specified.

did_multiplegt_stat lngca id year tau, or(1)  estimator(aoss waoss) estimation_method(dr)  placebo(1) aoss_vs_waoss by_fd(2)

// CC: I am getting an error "variable SIbist_XX already defined" when I do iv-waoss + by_fd

did_multiplegt_stat lngca id year lngpinc tau, or(1)  estimator(iv-waoss) controls(lngpinc) estimation_method(dr)  placebo(1) by_fd(2)

did_multiplegt_stat lngca id year tau, or(1)  estimator(waoss) estimation_method(dr) placebo(1) by_baseline(5)

//2. Using estout even when submodels are estimated with  By Quantile and FD
eststo clear

glob nb_quantiles = 3
eststo model_quant: did_multiplegt_stat lngca id year tau, or(1)  estimator(aoss waoss) estimation_method(dr) placebo(1) by_baseline($nb_quantiles)


forvalues quantil = 1/$nb_quantiles{
	
	if (`quantil'==1) local store = "replace"
	else local store = "append"
	//Add the title
	local label_quantile ="`e(label_quantile`quantil')'"
	//Add the number of observations
	estadd local  nobs_quantile "`e(nobs_quantile`quantil')'", replace  
	
global Panel`quantil' "posthead(\addlinespace \midrule[2pt] \multicolumn{2}{c}{\bf{`label_quantile'}}\\\midrule[2pt] )"
esttab model_quant  using "model_quant.tex"  ,cells(b(fmt(%12.4fc)) se(fmt(%12.4fc) par) ci(fmt(%12.4fc) par)) stats( nobs_quantile, fmt(%12.0fc)  labels("Observations") ) ///
/*addnote("Standard errors in parenthesis." "Confidence intervals in brackets.")*/ keep(AOSS`quantil' WAOSS`quantil' ) label ///
${Panel`quantil'} `store' nonumber nodep nomtitles type nofloat
}


forvalues quantil = 1/$nb_quantiles{
	
	if (`quantil'==1) local store = "replace"
	else local store = "append"
	local label_quantile ="`e(label_quantile`quantil')'"
	
global Panel`quantil' "posthead(\addlinespace \midrule[2pt] \multicolumn{2}{c}{\bf{Panel `quantil': `label_quantile'}}\\\midrule[2pt] )"
esttab model_quant  using "model_quant.tex"  ,cells(b(fmt(%12.4fc)) se(fmt(%12.4fc) par) ci(fmt(%12.4fc) par)) stats(N, fmt(%12.0fc)  labels("Observations") ) ///
/*addnote("Standard errors in parenthesis." "Confidence intervals in brackets.")*/ keep(AOSS`quantil' WAOSS`quantil' PlaceboAOSS`quantil' PlaceboWAOSS`quantil' ) label ///
${Panel`quantil'} `store' nonumber nodep nomtitles type nofloat
}

//Note: Put it as vignette to show users how to store results with estout with advanced options



/******************************************************************************
	   IV. EXACT MATCH: did_multiplegt_stat vs did_multiplegt
******************************************************************************/
bcuse wagepan, clear

//1. Binary treatment, and illustrating speed gains

timer clear
timer on 1
did_multiplegt_stat lwage nr year union, estimator(waoss)  exact_match placebo(1)
timer off 1
timer list 1

timer clear
timer on 1
did_multiplegt lwage nr year union, breps(100) placebo(1)
timer off 1
timer list 1

//2. Discrete treatment
gen dis_u = runiformint(1, 5)+union
gen lwage2  = lwage + 3*dis_u
did_multiplegt_stat lwage2 nr year dis_u, estimator(waoss aoss)  exact_match placebo(1)
did_multiplegt lwage2 nr year dis_u, breps(5)  placebo(1)

did_multiplegt_stat lwage2 nr year dis_u union, estimator(iv-waoss)  exact_match placebo(1)

//3. multiple treatments
xtset nr year 

gen othertreat = (lwage>1.5)
gen othertreat2 = (lwage<1)|(lwage>2)
tab othertreat*

did_multiplegt_stat lwage nr year union, estimator(waoss) placebo(1) other_treatments(othertreat othertreat2) exact_match 

xtset nr year
gen fd_othertreat = D.othertreat
gen fd_othertreat2 = D.othertreat2
tab fd_othertreat*

egen othertreat_ = group( othertreat2 othertreat )
did_multiplegt lwage nr year union, if_first_diff(fd_othertreat==0&fd_othertreat2==0) trends_nonparam(othertreat_) always_trends_nonparam breps(2) placebo(1)
