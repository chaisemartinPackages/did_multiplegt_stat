/***********************************************************************
	  I.  GAZOLINE DATA TESTING DIFFERENT OPTIONS
***********************************************************************/
cd "C:\Users\doulo\Dropbox (Personal)\SciencesPo\RAs Really Credible DID-TWFE\did_multiplegt_stat\Version_1"
cap prog drop did_multiplegt_stat
qui do "did_multiplegt_stat_ds.ado"
use "gazoline_did_multiplegt_stat.dta", clear

// Comment Felix: Change the displaying? -> group the placebos into one table for Aoss/Was and add some space between Aoss and Was Table
// Some missing values generated and real changes made output is not qui -> search for the gen/replace command that causes that SOLVED
// weights in helpfile, weight in syntax SOLVED
// weight does not work -> varname unkown weight type SOLVED
// Why do we always have this graph with 0 effect? SOLVED
// There seems to be a problem with globals (I assume) -> If you run something with iv-was or as_vs_was (I think, do not remember 100%) and it crashes it is like this option remains being called without specifying it
// Actuall with iv-was it could just be the problem of specifying the Z variable and not specifying the estimator
// Error message when putting fith Z variable but also specifying AS/WAS 
// As of now we get the first stage and then in the second table the (Non-IV) AS/WAS which is not what we want I guess
// We do not get a graph with the iv-was results
// Adjust reported number of switchers/stayers when using weights

//Reproducing first-stage and reduced-form from paper
did_multiplegt_stat lngca id year tau, or(1)  controls(lngpinc) estimator(as was)  placebo(3)  as_vs_was
did_multiplegt_stat lngca id year tau, or(2)  controls(lngpinc) estimator(as was)  placebo(3)  as_vs_was
did_multiplegt_stat lngpinc id year tau, or(1)  controls(lngpinc) estimator(as was)  placebo(3)  as_vs_was
did_multiplegt_stat lngpinc id year tau, or(2)  controls(lngpinc) estimator(as was)  placebo(3)  as_vs_was

// Reproducing iv from paper
did_multiplegt_stat lngca id year lngpinc tau, controls(lngpinc) or(1) estimator(iv-was) estimation_method(dr) 
did_multiplegt_stat lngca id year lngpinc tau, controls(lngpinc) or(2) estimator(iv-was) estimation_method(dr)

// CC: we need to fix graph issue when only one estimator is requested.
// CC: for later: we do not normalize graph at 0: effect shown at 0, as in did_multiplegt, and we keep the same legend as did_multiplegt "Time relative to treatment change (t=0)". -> Q FK: Does thia mean we put reference period at -1 or that we do not show reference period?
// Diego: solved issue 1.
did_multiplegt_stat lngpinc id year tau, or(2)  controls(lngpinc) estimator(was) placebo(3)

// GVB: we should get an error message when we specify negative polynomial order or a 0 polynomial order: polynomial order should be >=1
did_multiplegt_stat lngpinc id year tau, or(-1)  controls(lngpinc) estimator(was)  placebo(3)
did_multiplegt_stat lngpinc id year tau, or(0)  controls(lngpinc) estimator(was)  placebo(3)
// Diego: solved.

//Bootstrap + iv-was
did_multiplegt_stat lngca id year lngpinc tau, or(1)  estimator(iv-was) estimation_method(dr) bootstrap(2)
did_multiplegt_stat lngca id year lngpinc tau, or(1)  estimator(iv-was) estimation_method(dr) bootstrap(2) seed(1)
did_multiplegt_stat lngca id year lngpinc tau, or(1)  estimator(iv-was) estimation_method(dr) bootstrap(2) seed(1)

//weights
did_multiplegt_stat lngca id year tau, or(1) weight(lngpinc) estimator(was) estimation_method(dr) placebo(2)
did_multiplegt_stat lngca id year tau, or(1) estimator(was) estimation_method(dr) placebo(2)

// more than one control variable
did_multiplegt_stat lngca id year tau, or(1)  controls(lngpinc lncars) estimator(as was)  placebo(3)  as_vs_was
did_multiplegt_stat lngca id year tau, or(2)  controls(lngpinc lncars) estimator(as was)  placebo(3)  as_vs_was

// estimation methods
did_multiplegt_stat lngca id year tau, or(1) estimator(was) estimation_method(ps) placebo(2)
did_multiplegt_stat lngca id year tau, or(1) estimator(was) estimation_method(ra) placebo(2)
did_multiplegt_stat lngca id year tau, or(1) estimator(as) estimation_method(ps) placebo(2)

did_multiplegt_stat lngca id year tau, or(1) estimator(was) estimation_method(ra dr) placebo(2)
// Felix: Maybe add to the error message that only one at a time is allowed

// up/down
did_multiplegt_stat lngca id year tau, or(1) estimator(was) switchers(up)
did_multiplegt_stat lngca id year tau, or(1) estimator(was) switchers(down)

// bysort:
// CC: bysort does not do anything, needs to be fixed.
gen Democrat_Gov1966_temp=Democrat_Gov if year==1966
bys state: egen Democrat_Gov1966=sum(Democrat_Gov1966_temp)
bysort Democrat_Gov1966: did_multiplegt_stat lngca id year tau, or(1) estimator(was) graph_off
did_multiplegt_stat lngca id year tau, or(1) estimator(was)
bysort Democrat_Gov1966: did_multiplegt_stat lngca id year tau, or(1) estimator(was)
// Diego: solved.

//No extrapolation
did_multiplegt_stat lngca id year tau, or(1) estimator(was) switchers(up) noextra
did_multiplegt_stat lngca id year tau, or(1) estimator(was) switchers(up) 

//cluster 
gen cluster_id_letter=substr(state,1,1) 
did_multiplegt_stat lngca id year tau, or(1) controls(lngpinc) estimator(was) estimation_method(dr) placebo(1) cluster(cluster_id_letter)
did_multiplegt_stat lngca id year tau, or(1) controls(lngpinc) estimator(was) estimation_method(dr) placebo(1)

// DC spotted an instance where, with clustering, standard error of point estimate changes with and without a placebo, but this is not the case here.
did_multiplegt_stat lngca id year tau, or(1) controls(lngpinc) estimator(was) estimation_method(dr) cluster(cluster_id_letter)

//twfe vs was + percentile bootstrap + same_sample, and was vs as
// CC: for later: maybe better to report the TWFE estimator in fixed sample, rather than average of TWFEs across bootstrap samples. -> FK: only bootstrap the DIfference, keeping fixed sample AS/WAS but TWFE from different samples will be confusing
// CC: for later: perhaps make it possible to specify twfe without any option.

did_multiplegt_stat lngca id year tau, or(1)  estimator(as was) estimation_method(dr)  bootstrap(5) placebo(1) twfe(percentile same_sample) as_vs_was

//twfe vs was + normal bootstrap + same_sample, and was vs as
did_multiplegt_stat lngca id year tau, or(1)  estimator(as was) estimation_method(dr)  bootstrap(5) placebo(1) twfe(same_sample) as_vs_was

//2sls-twfe vs iv-was
did_multiplegt_stat lngca id year lngpinc tau , or(1)  estimator(iv-was) estimation_method(dr)  bootstrap(5) placebo(1) twfe(percentile same_sample)

//Cross_validation
// GVB: the cross-validation option should not give a warning message about the order when order is not specified:

did_multiplegt_stat lngca id year tau, estimator(as was) placebo(1) estimation_method(dr)  cross_validation(algo(loocv) tole(0.01) max_k(3) seed(1) kfolds(2)) 
// Felix: there is no message when or(1) is specified!!!


// CC: the cross validation option does not seem compatible with the by_baseline option, we will fix this later, 
// for now I just indicated it in the help file, and we just need to shut down this combination of options and give an error message when specified together. SOLVED by this sreturn overwriting issue and a typo in cap drop
did_multiplegt_stat lngca id year tau, or(1) estimator(as was) placebo(1) estimation_method(dr)  cross_validation(algo(loocv) tole(0.01) max_k(2) seed(1) kfolds(2)) by_baseline(5)
did_multiplegt_stat lngca id year tau, or(1) estimator(as was) placebo(1) estimation_method(dr)  cross_validation(algo(loocv) tole(0.01) max_k(2) seed(1) kfolds(2)) by_fd(2) //Doulo: I guess it is because the cv routine came before the by quantile one. Need to switch their positions. To do after.

/***********************************************************************
	   II. GAZOLINE DATA ESTOUT OPTION
***********************************************************************/
eststo clear

//Panel A: Reduced-form effect of taxes on quantities consumed.

global PanelA "posthead(\addlinespace \midrule[2pt] \multicolumn{3}{c}{\bf{Panel A: Reduced-form effect of taxes on quantities consumed}}\\\midrule[2pt] )"

global PanelAp "posthead(\addlinespace \midrule[2pt] \multicolumn{3}{c}{\bf{Panel A: Reduced-form placebo(1) effect of taxes on quantities consumed}}\\\midrule[2pt] )"
//1. Linear model
eststo linearReducedForm: did_multiplegt_stat lngca id year tau, or(1) estimation_method(dr) as_vs_was placebo(1) controls(lngpinc)

// CC: for later: the command seems to not produce a scalar pval_XX anymore, is that normal? Doulo: SOLVED
estadd scalar pvalue = scalar(pval_as_vs_was)
estadd local  control "\checkmark", replace  

//2. Quadratic model
eststo quadReducedForm:  did_multiplegt_stat lngca id year tau, or(2) estimation_method(dr) as_vs_was placebo(1) controls(lngpinc)

// CC: for later: the command seems to not produce a scalar pval_XX anymore, is that normal? Doulo: SOLVED
estadd scalar pvalue = scalar(pval_as_vs_was), replace
estadd local  control "\checkmark", replace 

//Effect Panel A
esttab linearReducedForm quadReducedForm  using "AS_WAS_Effects_controls.tex"  ,cells(b(fmt(%12.4fc)) se(fmt(%12.4fc) par) ci(fmt(%12.4fc) par)) stats(N pvalue control, fmt(%12.0fc %12.4fc 0 )  labels("Observations" "\bf{p.value}" "Controls") ) ///
mtitles("(1) Linear model" "(2) Quadratic model") nonumber collab(none) ///
/*addnote("Standard errors in parenthesis." "Confidence intervals in brackets.")*/ keep(AS WAS) label ///
${PanelA} replace

//Placebo Panel A
esttab linearReducedForm quadReducedForm  using "AS_WAS_Placebos_controls.tex"  ,cells(b(fmt(%12.4fc)) se(fmt(%12.4fc) par) ci(fmt(%12.4fc) par)) stats(N control, fmt(%12.0fc 0 )  labels("Observations" "Controls") ) ///
mtitles("(1) Linear model" "(2) Quadratic model") nonumber collab(none) ///
/*addnote("Standard errors in parenthesis." "Confidence intervals in brackets.")*/ keep(PlaceboAS PlaceboWAS) label ///
${PanelAp} append

/******************************************************************************
	   III. By Quantile and FD
******************************************************************************/

//1. The command

// CC: for later: graphs by fd get erased by ES graph when by_fd is specified. We do not do ES graph when by_fd or by_baseline specified.

did_multiplegt_stat lngca id year tau, or(1)  estimator(as was) estimation_method(dr)  placebo(1) as_vs_was by_fd(2)

// CC: I am getting an error "variable SIbist_XX already defined" when I do iv-was + by_fd
// Felix: Same for by_baseline -> SOLVED, typo in cap drop


did_multiplegt_stat lngca id year lngpinc tau, or(1)  estimator(iv-was) controls(lngpinc) estimation_method(dr)  placebo(1) by_fd(2)

did_multiplegt_stat lngca id year tau, or(1)  estimator(was) estimation_method(dr) placebo(1) by_baseline(5)

//2. Using estout even when submodels are estimated with  By Quantile and FD
eststo clear

glob nb_quantiles = 3
eststo model_quant: did_multiplegt_stat lngca id year tau, or(1)  estimator(as was) estimation_method(dr) placebo(1) by_baseline($nb_quantiles)


forvalues quantil = 1/$nb_quantiles{
	
	if (`quantil'==1) local store = "replace"
	else local store = "append"
	//Add the title
	local label_quantile ="`e(label_quantile`quantil')'"
	//Add the number of observations
	estadd local  nobs_quantile "`e(nobs_quantile`quantil')'", replace  
	
global Panel`quantil' "posthead(\addlinespace \midrule[2pt] \multicolumn{2}{c}{\bf{`label_quantile'}}\\\midrule[2pt] )"
esttab model_quant  using "model_quant.tex"  ,cells(b(fmt(%12.4fc)) se(fmt(%12.4fc) par) ci(fmt(%12.4fc) par)) stats( nobs_quantile, fmt(%12.0fc)  labels("Observations") ) ///
/*addnote("Standard errors in parenthesis." "Confidence intervals in brackets.")*/ keep(AS`quantil' WAS`quantil' ) label ///
${Panel`quantil'} `store' nonumber nodep nomtitles type nofloat
}


forvalues quantil = 1/$nb_quantiles{
	
	if (`quantil'==1) local store = "replace"
	else local store = "append"
	local label_quantile ="`e(label_quantile`quantil')'"
	
global Panel`quantil' "posthead(\addlinespace \midrule[2pt] \multicolumn{2}{c}{\bf{Panel `quantil': `label_quantile'}}\\\midrule[2pt] )"
esttab model_quant  using "model_quant.tex"  ,cells(b(fmt(%12.4fc)) se(fmt(%12.4fc) par) ci(fmt(%12.4fc) par)) stats(N, fmt(%12.0fc)  labels("Observations") ) ///
/*addnote("Standard errors in parenthesis." "Confidence intervals in brackets.")*/ keep(AS`quantil' WAS`quantil' PlaceboAS`quantil' PlaceboWAS`quantil' ) label ///
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
did_multiplegt_stat lwage nr year union, estimator(was)  exact_match placebo(1)
timer off 1
timer list 1

timer clear
timer on 1
did_multiplegt_old  lwage nr year union, breps(100) placebo(1)
timer off 1
timer list 1

//2. Discrete treatment
gen dis_u = runiformint(1, 5)+union
gen lwage2  = lwage + 3*dis_u
did_multiplegt_stat lwage2 nr year dis_u, estimator(was as)  exact_match placebo(1)
did_multiplegt_old lwage2 nr year dis_u, breps(5)  placebo(1)

did_multiplegt_stat lwage2 nr year dis_u union, estimator(iv-was)  exact_match placebo(1)

//3. multiple treatments
xtset nr year 

gen othertreat = (lwage>1.5)
gen othertreat2 = (lwage<1)|(lwage>2)
tab othertreat*

did_multiplegt_stat lwage nr year union, estimator(was) placebo(1) other_treatments(othertreat othertreat2) exact_match 

xtset nr year
gen fd_othertreat = D.othertreat
gen fd_othertreat2 = D.othertreat2
tab fd_othertreat*

egen othertreat_ = group( othertreat2 othertreat )
did_multiplegt_old lwage nr year union, if_first_diff(fd_othertreat==0&fd_othertreat2==0) trends_nonparam(othertreat_) always_trends_nonparam breps(2) placebo(1)



/******************************************************************************
	   V. Additional tests Felix
******************************************************************************/

cd "C:\Users\fe-kn\Desktop\RA ScPo\GitHub_working\did_multiplegt_stat"

qui do "did_multiplegt_stat.ado"
use "gazoline_did_multiplegt_stat.dta", clear

//// Testing combination of options ////
did_multiplegt_stat lngca id year tau, or(1) controls(lngpinc) estimator(as was) placebo(3) as_vs_was switchers(stupid comment)
// FK: warning or error message when specifying something else than up/down with the switchers option

did_multiplegt_stat lngca id year tau, or(1) controls(lngpinc) estimator(as was) placebo(3) as_vs_was by_fd(1)
// FK: one quantile with 0 observations, is this ok or not?
// FK: by_fd(1) equivallent to not putting this option (makes sense), by_fd(0) the same (does not make that much sense)

did_multiplegt_stat lngca id year tau, or(1) controls(lngpinc) estimator(as was) placebo(3) as_vs_was by_baseline(0)
// FK: Same as the second point for by_fd

did_multiplegt_stat lngca id year lngpinc tau, or(1) controls(lngpinc) estimator(iv-was) placebo(3) bootstrap(2) seed(1)
// FK: allows negative input, handeled as if no bootstrap option, produce error message instead SOLVED
// FK: bootstrap(1) produces by definition missing se which leads to an error as the e(V) matrix has missing value
// FK: same for seed, however I do not know if stata can interprete negative seeds in a meaningful way -> according to the helpfile it shouldbe >=0 SOLVED

gen cluster_id_letter_1=substr(state,1,1) 
gen cluster_id_letter_2=substr(state,2,1) 

did_multiplegt_stat lngca id year tau, or(1) estimator(was) placebo(3) other_treatments(lngpinc road_mileage)
// FK: combinations lead to empty samples, however I think that is not an error of the program but due to a "useless" specification

did_multiplegt_stat lngca id year tau, or(1) estimator(was) placebo(3) twfe(percentile) bootstrap(10)
// FK: Be more precise in the help file -> you always need to put twfe() and not just twfe, even without any suboptions
// FK: Maybe 100 bootstrap replications are a bit too much by default, so also mention that you can do less replications by specifying the bootstrap option 
// FK: twfe does not run by default, you have to specify either percentile or same_sample
// FK: Also produces error message when you try to put twfe() without arguments with bootstrap option


did_multiplegt_stat lngca id year tau
// FK: produces AS, WAS, and as_vs_was by default, is this what we want? -> somehow as_vs_was not there anymore???

/* SOLVED
did_multiplegt_stat lngca id year tau, estimator(as [aw=hug] was)
// FK: Is this a feature that it works or should it throw w warning/error ([aw=hug] is just placeholder for anytihing we could put there that will be ignored)
did_multiplegt_stat lngca id year tau, estimator(test test test as)
// the program is only checking the first three arguments 
*/

did_multiplegt_stat lngca id year tau, estimator(as was) estimation_method(ra t dr)
// FK: here we have an error message as I would have expected

did_multiplegt_stat lngca id year tau, estimator(as was) placebo(60)
// FK: crashes with a too large number of placebos -> invalid syntax
// add a checking mechanism like in dyn -> Not as straight forward to determine the max number of placebos (compared to the setting with dynamic effects), T-2 can serve as the max upper bound, but not efficient I guess.

did_multiplegt_stat lngca id year lngpinc tau, or(1) weights(lngpinc) estimator(was as iv-was) estimation_method(dr) placebo(2)
// FK: provides correct error message (AS or WAS cannot be combined with the estimation of IV-WAS), but only after running the first stage -> is this intended or put error message at the beginning?

//// CV options

// inside the description of the max_k option in the help file it is not really clear what is done if the tolerance is not reached (use max/min/whatever) -> from the code I interfere we chose order 1
// Also mention somewhere in the help file that the "outer option" is called cross_validation(), because there we only see the suboptions
did_multiplegt_stat lngca id year tau, estimator(was) estimation_method(dr) cross_validation(algorithm(kfolds)) or(2)
// error as one of the locals that should be created in the inner cross_validation program does not exist (or is empty)

did_multiplegt_stat lngca id year tau, estimator(was) estimation_method(dr) cross_validation(/*algo(loocv) tole(0.01) seed(1) kfolds(2) max_k(3)*/) or(2)
// It looks like the the default in algorithm and max_k are not set correctly as they cause errors when they are not explicitely specified 


//// How does cv deal with inputs when kfold/algo is specified or not 
cap program drop did_multiplegt_stat
qui do "did_multiplegt_stat_ds.ado"
// loocv +
did_multiplegt_stat lngca id year tau, estimator(as was) placebo(1) estimation_method(dr)  cross_validation(algo(loocv) tole(0.05) max_k(5) seed(1))

// loocv + kfolds
did_multiplegt_stat lngca id year tau, estimator(as was) placebo(1) estimation_method(dr)  cross_validation(algo(loocv) tole(0.01) max_k(3) seed(1) kfolds(5)) 

// kfolds + kfolds
did_multiplegt_stat lngca id year tau, estimator(as was) placebo(1) estimation_method(dr)  cross_validation(algo(kfolds) tole(0.01) max_k(3) seed(1) kfolds(5)) 

// + kfolds
did_multiplegt_stat lngca id year tau, estimator(as was) placebo(1) estimation_method(dr)  cross_validation(tole(0.01) max_k(3) seed(1) kfolds(5)) 
// here it is not clear what is happening and the results differ from kfolds + kfolds (not what I expected, thought it would be as specifying algo(kfolds), maybe I should add that default initialization somewhere). Everything else seems reasonable
// -> Idea: For both cross_validation and twfe pick one of the suboption the user HAS TO SPECIFY. Make this very clear in the helpfile! For cross_validation it will be algorithm, for twfe the bootstrap version (like add a standard in addition to just percentile).


//// Combine all options 
did_multiplegt_stat lngca id year tau, estimator(as was) placebo(1) estimation_method(dr)  cross_validation(algo(kfolds) tole(0.01) max_k(3) seed(1) kfolds(3)) controls(hug fgastax) switchers(up) by_baseline(2) as_vs_was or(2) noextra twfe(percentile) bootstrap(3) disag weights(taxin_gas_price)
// With all those options there is output (from the cross validation) between the "bootstrap header" and the progress dots!




///// List Felix /////
/*

- Default options: for cross_validation the use should specify the algorithm, everything else is then by default as described in the helpfile. For twfe my idea is to add some kind of method() suboption instead of percentile that you have to specify, and then in there you have to put "classical" or percentile -> DONE
did_multiplegt_stat lngca id year tau, estimator(was) placebo(1) estimation_method(dr) cross_validation(algo(loocv)) 

-> for twfe make same_sample vs. full_sample and you have to specify one of them -> DONE
did_multiplegt_stat lngca id year tau, or(1) estimator(was) placebo(3) twfe(full_sample) bootstrap(10)

- Also explain in the helpfile more detailed how to put syntax with the suboptions (we do not even mention the exact cross_validation() or twfe() outer syntax) -> DONE

- "Fish-Plot" for the Placebos 

- With the by_fd and by_baseline option we supress the "event-study" plot, maybe do this by setting the graph_off option on? -> DONE 
- Alternative: save the by_fd/by_baseline graphs under specific name and get both in this way?
did_multiplegt_stat lngca id year tau, or(1) estimator(as was) placebo(3) as_vs_was by_fd(5)

- Group placebo tables by AS/WAS and add some space inbetween -> DONE
did_multiplegt_stat lngca id year tau, or(1) estimator(as was) placebo(10) as_vs_was

- Error message when putting fith Z variable but also specifying AS/WAS -> at the moment you do a first stage and then AS/WAS estimators (with for some reason missing results), no idea what is going on there 

-> when we have 5 arguments overwrite estimator by (iv-was) with warning message -> DONE
did_multiplegt_stat lngca id year lngpinc tau, or(1) estimator(was as) estimation_method(dr) placebo(2)

- When you specify estimator(iv-was as was) you get an error message, but only after the first stage is run -> put this at the beginning? -> DONE as this is not an issue anymore due to the automatic overwriting of the estimator by iv-was in that case

- We do not get a graph with the iv-was results -> that is the one we wan
 at the end -> DONE 

- Adjust reported number of switchers/stayers when using weights -> for later

- Warning or error message when specifying something else than up/down with the switchers option -> DONE
did_multiplegt_stat lngca id year tau, or(1) controls(lngpinc) estimator(as was) placebo(3) as_vs_was switchers(donw)

- Maybe 100 bootstrap replications (default with twfe) are a bit too much by default, so also mention that you can do less replications by specifying the bootstrap option -> look at bootstrap speed for later

- crashes with a too large number of placebos -> invalid syntax
add a checking mechanism like in dyn -> Not as straight forward to determine the max number of placebos (compared to the setting with dynamic effects), T-2 can serve as the max upper bound, but not efficient I guess.

-> look at the last period where we still have a switch minus 2 -> DONE
did_multiplegt_stat lngca id year tau, or(1) estimator(was) placebo(100)

// New additions Clement //

- only show event-study graph when at least one placebo requested -> DONE
did_multiplegt_stat lngca id year tau, or(1) estimator(as was) as_vs_was

- In the table, replace "weighted Average Slopes (WAS)" by "Weighted Average Slope (WAS)", and replace " Average Slopes (AS)" by " Average Slope (AS)" -> DONE
did_multiplegt_stat lngca id year tau, or(1) estimator(as was) placebo(1) as_vs_was

- when we request AS and WAS, the title of the event-study graph "DID from last period..." is misleading, suggests that estimators = long diffs, please just drop this title -> DONE
did_multiplegt_stat lngca id year tau, or(1) estimator(as was) placebo(1) as_vs_was

*/


///// List of more elaborate adjustements for did_multiplegt_stat later /////

/*

- we do not normalize graph at 0: effect shown at 0, as in did_multiplegt, and we keep the same legend as did_multiplegt "Time relative to treatment change (t=0)" -> this in combination with the new idea with the fish skelleton type of plot

- report the TWFE estimator in fixed sample, rather than average of TWFEs across bootstrap samples

- the command seems to not produce a scalar pval_XX anymore, is that normal -> FK: should this be the as_vs_was p-value?
Doulo: This is due to the program drop_scalars dropping all scalar with pattern _XX; solution: rename pval_XX by smtg else, e.g. pval_as_vs_was: Doulo: SOLVED

-> added a few lines in the main program to drop all scalars. Two possible solutions: ereturn the full as_vs_was matrix or just store the p-value as ereturn scalar (more fitting to store the full matrix I would say): Doulo: SOLVED

- put it as vignette to show users how to store results with estout with advanced options

- adjust reported number of switchers/stayers when using weights (like we did in dyn)

- fix the graphs that are shown twice before being combined into graph with AS and WAS, probably slows down command.
Doulo: SOLVED.

- bootstrap very slow when twfe option specified, check what's going on

- ask Doulo to show an example (perhaps based on simulated data) where CV does something and select polynomial orders >1. Here, model k=1 and k=2 always give the exact same CV score, to the number of digits shown, a bit worrying (Doulo: I think I know where the problem is coming from: look at polK. I will solve it before the meeting.). Also, "convergence not achieved" issue disappears when "by_fd(2)" is specified, a bit surprising

*/