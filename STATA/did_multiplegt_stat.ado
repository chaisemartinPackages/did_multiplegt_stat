*This program estimates the three estimators (as, was, iv-as) developped in 
**de Chaisemartin, Clément and d'Haultfoeuille, Xavier and Pasquier, Félix and Vazquez‐Bare, Gonzalo,
** Difference-in-Differences Estimators for Treatments Continuously Distributed at Every Period (January 18, 2022). 
**Available at SSRN: https://ssrn.com/abstract=4011782 or http://dx.doi.org/10.2139/ssrn.4011782

//Doulo: on Jan, 4: this versions compute the as - was - iwas estimators, and theirs variances with options (polynom, switchers)

//1. The simulations made with this version look good: ~96% of coverage, with 100 simulations (test_simu_madeupData1.do)

//2. At this date, Janv 4, the command does not consider the logit, and the doubly_robust option,  appraoches yet.

//3. The number of periods is >=2

//4. Missing values problem is solved - unbalanced data

//Main options: estimator(as|was|iwas), estimation_method(ra|ps|dr), order(), switchers(up|down), noextrapolation, placebo, disaggregate


*The aggregation is done, and is well working: as test I tried to also to test whether I have the same variance if T=2 for both the aggregated and the simple case. 
*noextrapolation coded
		
//To be added checked: Consider the difference combination (as, was, iwas) in if conditions //solved

//Janv, 15: the new versions of the IFs are implemented

//panel data with gaps or one type cases are also coded

//Janv, 17 I started coding the ps-based approach for the was, starting from the ra-based approach version
//I added the dr-based approach

//Janv, 29: Some adjustements to wrap up: 
	/* 1. For the as, \hat{E}(S|D_1) in the influence function is no longer estimated by a linear regression but by a logit 
	/All the methods (ra, ps, dr) are computed for as, was, and iwas as well. Need to do the aggreagation for iwas (done!).
	*/
//Janv 30, the aggregated and disaggregated placebos versions are computed.

///FEBRUARY, 1: This version computes all the options provided in the syntax, except only the aggreagation of IWAS.
//FEB, 8: The command is more or less done, I made several checks (coverage rate, influence functions, points estimate, sanity-checks etc.) and we are find so far.

//Feb, 19: Option cluster is computed. 

//Feb 21: The weights option: The idea is (i) to exploit the command {sum varlist [iw = weights_XX]}, (ii) put the option weights in all the regressions (logit and linear), (iii) instead of usingg r(N) use r(sum_w) etc.

//Feb, 23: Shut down the option weights and cluster: local weights = "" and local cluster = ""

//The program is now byable and produces a graph of the effects and the placebos (see bys_graph_off)


//Next: Add the baseline|FD by option (done):  pctile newvar = oldvar, nq(#) 
// March 11, FD and baseline option computed 
//Multiple treatment option computed. 
//Cluster option ok
//Adding controls ok
//weighs ok
//Add Placebo FS placebo : okay, the program shows the FS whenever IV is requested ok
//Adding cross_validation ok


//// TO DO LIST
// 1. Different tables for each version of placebo
// 2. Graph like in did_multiplegt_dyn showing placebos and Effect_0, with x-axis being the periods.

capture program drop did_multiplegt_stat
program did_multiplegt_stat, eclass sortpreserve byable(recall)
	version 12.0
	syntax varlist(min=4 max=5 numeric) [if] [in] [, estimator(string) estimation_method(string) ORder(integer 1) NOEXTRApolation placebo(integer 0) switchers(string) DISAGgregate as_vs_was exact_match bys_graph_off by_fd(integer 1) by_baseline(integer 1) other_treatments(varlist numeric) cluster(varlist max=1) controls(varlist numeric) weights(varlist numeric max=1)  bootstrap(integer 0) seed(integer 0) twfe(string) cross_validation(string) graph_off ] 

	marksample touse // Felix This causes the program to crash with weightss
	if _by() {
		quietly replace `touse' = 0 if `_byindex' != _byindex()
	}
	
/// Modif Felix: Add some kind of preamble to ensure "meaningful" inputs ///

// bootstrap has to be POSITIVE integer
if `bootstrap'<0{
	di ""
	di as error "The input of the bootstrap() option has to be a positive integer!"
	di as input _continue ""
	exit
}

// seed has to be POSITIVE integer
if `seed'<0{
	di ""
	di as error "The input of the seed() option has to be a positive integer!"
	di as input _continue ""
	exit
}

// switchers has to be up or down (or empty)
if ("`switchers'"!="up" & "`switchers'"!="down" & "`switchers'"!=""){
	di ""
	di as error "The input of the switchers() option has to be either up or down!"
	di as input _continue ""
	exit
}

	//tokenize the varlist
tokenize `varlist'

if ("`5'"!=""&"`5'"!=","){
local IV_feed_XX = "yes"
local IV_var_XX  `5' 

// Modif Felix: add warning at the beginning
if "`estimator'"!="iv-was"{
		di ""
		di as error "You specified the 5th input variable (Instrumental variable) which is only compatible with the iv-was estimator."
		di as error "Therefore your estimator will be replaced by iv-was."
		di as input _continue ""
	}

//Show the First Stage
		di as input "{hline 80}"
		di as input _skip(30) "First stage estimation"
		di as input "{hline 80}"
		//if strpos("`5'", ",") != 0 local 5 = strtrim(substr("`5'", 1, strpos("`5'", ",") - 1))
		local estimator = "was"
		did_multiplegt_stat2 `4' `2' `3' `5' if `touse' == 1,  estimator(`estimator') estimation_method(`estimation_method') order(`order') `noextrapolation' placebo(`placebo') switchers(`switchers') `disagregate' `as_vs_was' `exact_match' `bys_graph_off' by_fd(`by_fd') by_baseline(`by_baseline') other_treatments(`other_treatments') cluster(`cluster') controls(`controls') weights(`weights') cross_validation(`cross_validation') `graph_off' //twfe(`twfe')
}


//Show the main results
		local cmd = subinstr("`0'", ",", " if `touse' == 1,", 1)
		did_multiplegt_stat2 `cmd'
				
		//Drop scalar created by the program
		cap scalars_to_drop  //a subprogram dropping all scalars with the pattern _XX
		cap scalar drop aggregated_data  max_mT
		cap scalar drop N_var
		cap scalar drop set_chosen_order_linear
		cap scalar drop N_drop_noextra_XX 
		cap scalar drop max_T
end 


capture program drop did_multiplegt_stat2
program did_multiplegt_stat2, eclass sortpreserve byable(recall)
	version 12.0
	syntax varlist(min=4 max=5 numeric) [if] [in] [, estimator(string) estimation_method(string) ORder(integer 1) NOEXTRApolation placebo(integer 0) switchers(string) DISAGgregate as_vs_was exact_match bys_graph_off by_fd(integer 1) by_baseline(integer 1) other_treatments(varlist numeric) cluster(varlist max=1) controls(varlist numeric) weights(varlist numeric max=1)  bootstrap(integer 0) seed(integer 0) twfe(string) cross_validation(string) graph_off ] // FIRST_stage   twfe(percentile same_sample)

	
	// Felix: Do not show "event-study" graph with by_baseline or by_fd
	if (`by_baseline'!=1 | `by_fd'!=1){
		local graph_off="graph_off"
	}
	
	// Felix: Only show "event-study" graph when at least one placebo is requested
	if `placebo'==0{
		local graph_off="graph_off"
	}	
	
	//>MAIN: Preserve the inputted dataset
	preserve
	
	//If the user specify twfe without setting the bootstrap, take 100 as default.
	//And store all the suboptions of twfe: percentile, same_sample (default are normal and maybe different samples.)
	if ("`twfe'"!=""){ // Felix: This is a problem as twfe is a string (this wont get triggered then with twfe() plus for the same reason an error with the bootstrap option will be triggered because it is only allowed with twfe(which the command thinks is not specified)), what are you supposed to input as default -> force user to specify something, see helpfile
		parse_select_twfe_suboptions, `twfe'
		if (`bootstrap'==0) local bootstrap = 100
		local percentile "`s(percentile)'"
		local same_sample "`s(same_sample)'"
	}		
	
		//Bootstrap running bar Start	
if (`bootstrap'>0){
di _newline
_dots 0, title(Bootstrap running) reps(`bootstrap')
}
quietly{
//Shut down the option weights and cluster: 
//local weights = "" 
//local cluster = ""

// Check if gtools is installed, if not present link to install
qui cap which gtools
if _rc{
	di ""
	di as error "You have not installed the gtools package which is used within the did_multiplegt_stat command."
	di `"{stata "ssc install gtools": Click here to install gtools}"'
	exit
}	

//tokenize the varlist
tokenize `varlist'

//dropping observations not included in the if condition
	if "`if'" !=""{
	keep `if'
	}
// dropping observations not included in the in condition
	if "`in'" !=""{
	keep `in'
	} 


//capdrop varlist
cap drop Y_XX
cap drop D_XX
cap drop T_XX
cap drop ID_XX
cap drop weights_XX
cap drop weights_cXX
cap drop S_XX
cap drop D1_XX
cap drop deltaD_XX
cap drop deltaY_XX
cap drop to_drop_XX
cap drop tsfilled_XX
cap drop T_OG_XX


///// Collapse and weights
	
// Checking wether data has to be collapsed, because at a more disaggregated level than group*time.
 
capture drop counter_XX
capture drop counter_temp_XX
gen counter_temp_XX=1
bys `2' `3' : gegen counter_XX=count(counter_temp_XX)
sum counter_XX
scalar aggregated_data=0
if r(max)==1{
scalar aggregated_data=1
}

/*
// Collapsing the data when necessary:

if scalar(aggregated_data)==0{
	
	replace weights_XX=0 if `1'==.
	
	if "`1'"!="`4'"{
				//Averging the outcome and treatment
		collapse (mean) `1' `4' (count) weights_XX [iw=weights_XX], by(`2' `3')
	}
	
	if "`1'"=="`4'"{			
		collapse (mean) `1' (count) weights_XX [iw=weights_XX], by(`2' `3')
	}	
}
*/

//gen main varlist
gen Y_XX = `1'
gen ID_XX =  `2'
gen T_OG_XX = `3'
gen D_XX = `4' 
local depname  = "`1'" //for estout
local OG_nameID_XX = "`2'"
//2. IV method:
local IV_feed_XX = "no"

if ("`5'"!=""&"`5'"!=","){
local IV_feed_XX = "yes"
local IV_var_XX  `5' 

// Modif Felix: Replace the estimator we are actually using
	if "`estimator'"!="iv-was"{		
		local estimator = "iv-was" 
	}

}

 xtset `2'  `3' //make the quietly skip that to show the characteristics of the panel: balanced/unbalanced/w|o gaps etc.
/*******************************************************************************
//Check all the estimators that are requested - to customize the display
*******************************************************************************/

	local 1 ""
	local 2 ""
	local 3 ""
	
if("`estimator'" ==""){
	if ("`IV_feed_XX'" == "no"){
	local 1 "as"
	local 2 "was"
	//local 3 "iwas" //Now the default (when estimator is not specified) is to compute as and was only if Z is not given
	local as_XX = 1
	local was_XX = 1
	local iwas_XX= 0
	local estimator = "as was"  //Doulo: Set estimator values
	}
	else{
	//local 1 "as"
	//local 2 "was"
	local 3 "iv-was" //Now the default (when estimator is not specified) is to computes iwas only if Z is specify
	local as_XX = 0
	local was_XX = 0
	local iwas_XX= 1
	local estimator = "iv-was" //Doulo: Set estimator values
	}
}
else{
	//Count the number of estimators requested
	//scalar nb_estimatorts_XX = `:word count `estimator''
		tokenize `estimator'
		local as_XX = inlist("as", "`1'", "`2'", "`3'")
		local was_XX = inlist("was", "`1'", "`2'", "`3'")
		local iwas_XX= inlist("iv-was", "`1'", "`2'", "`3'")
		
		if ("`as_XX'"=="1"){
			local 1 = "as" 
		}
		if ("`was_XX'"=="1"){
			local 2 = "was" 
		}
		if ("`iwas_XX'"=="1"){
			local 3 = "iv-was" 
		}

}

local total_estimator = `as_XX' + `was_XX' + `iwas_XX'

// count the number of arguments in `estimator' compare them to total_estimator to see if there are other things specified that should not be
local nb_estimatorts_XX = `:word count `estimator'' //Doulo: Set estimator values above, in case estimnator is empty

/********************************************************************************
			ERRORS MESSAGES FOR :
********************************************************************************/

//0. Estimators
if (`total_estimator'!=`nb_estimatorts_XX'){ // Modif Felix, see above
		di as error "Error in the option estimator."
		di as error "The arguments allowed are: as, was, or iv-was." //Doulo: This is to restrictive. By default if the user does not specify estimator and Z, we should automatically produce as and was. And if he specifies we displays automatically iv-was. Solved above(See //Doulo: Set estimator values)
		exit
}
//1. Estimation method:

if ("`estimation_method'" == "ps"|"`estimation_method'" == "dr"){
	if ("`was_XX'"=="0"&"`iwas_XX'"=="0"){
		di as error "The propensity/doubly-robust -based approach is only available for the was and the iv-was."
		exit
	}
}
local req_est_method = inlist("`estimation_method'", "" , "ra", "ps", "dr")

if (`req_est_method' == 0){
		di as error "Error in the option estimation_method."
		di as error "The arguments allowed are: ra, ps, or dr."
		exit

}

//2. IV
if ("`IV_feed_XX'"=="no"&("`iwas_XX'" == "1")){
	di as error "To compute the iv-was you must specify the IV variable."
	exit
}


//3. Combination of estimators //Shut this constraint down: before I will need to modify the way the pairwise is called: We will need to call the pairwise program for each estimator and not once. Otherwise, if the user specifies for instance the option noextrapolation, the command will apply the two conditions (noextra on the iV and noextra on the treatment): Ask Clement?

if ("`iwas_XX'" == "1"&("`was_XX'" == "1"|"`as_XX'" == "1")){
	di as error "The estimation of AS or WAS cannot be combined with the estimation of IV-WAS (see helpfile)."
	exit
}


//4. The test of equality between as and was
local a_vs_w = `as_XX' + `was_XX'
if ("`as_vs_was'"!=""&`a_vs_w'!=2){
	di as error "To test the equility between AS and WAS you must specify as and was in the estimator option."
	exit
}

if !(mod(`order', 1)  == 0 & `order' > 0) {
	di as error "Order should be a positive integer"
	exit
}

//5.
if (`total_estimator'!=`nb_estimatorts_XX'){ // Modif Felix, see above
		di as error "Error in the option estimator."
		di as error "The arguments allowed are: as, was, or iv-was."
		exit
}

//6. exact_match and noextrapolation
if ("`exact_match'"!=""&"`noextrapolation'"!=""){
		di as error "As the exact_match option is specified,"
		di as error "the noextrapolation option is ignored."
	local noextrapolation = ""
}

//7. Order and cross_validation
if (`order'>1&"`cross_validation'"!=""){
	di as error ""
	di as error "The option order is not allowed along with cross-validation."
	di as error "The command will ignore the option order()."
	local order = 1
}

//8. exact_match and [order, estimation_method, order]
if ("`exact_match'"!=""){
	if ("`estimation_method'"!=""){
		di as error "As the exact_match option is specified,"
		di as error "the estimation_method option is ignored."
	}
	
	if (`order'!=1){ //The values are then set within the pairwise program.  To se where search local order = r(r)
		di as error "As the exact_match option is specified,"
		di as error "the order option is ignored."
	}
local estimation_method = "ra"	//set the default if exact_match>
	
}
else{
	
	//Set the default to 1 if the option is not specified and without exact_match
	// Modif. -> now the default is always 1
	/*
	if (`order'==0){
		local order =1 
	}
	*/
	if ((`was_XX'==1|`iwas_XX'==1)&"`estimation_method'"==""){
		local estimation_method = "dr" //default option
		
	}
}

//9. Cluster non-nested  //CLUSTER OPTION
if ("`cluster'"!=""&"`cluster'"!="`OG_nameID_XX'"){
	cap drop cluster_numeric_XX 
	egen cluster_numeric_XX = group(`cluster')
	bysort ID_XX: gegen cluster_sd_XX = sd(cluster_numeric_XX)
	sum cluster_sd_XX
	scalar max_cluster_sd_XX = `r(max)'
	if (scalar(max_cluster_sd_XX) >0){
	di as error ""
	di as error "The ID variable should be nested within the clustering variable."
	exit
	}
}	

//10.Bootstrap is only allowed with iv-was
if (`bootstrap'!=0&`iwas_XX'==0&"`twfe'"==""){
	di as error ""
	di as error "The bootstrap option is only available for the iv-was,"
	di as error "or combined with twfe."
	di as error "Please run the command without that option."
	exit
}
if (`seed'!=0&`bootstrap'==0){
	di as error ""
	di as test "Warning: The seed option is only relevant when specified with bootstrap."
}	

********************************************************************************

//Handle missing values i)
//gen to_drop_XX = (Y_XX==.|T_OG_XX==.|D_XX==.|ID_XX==.)
//Handle missing values ii)
if ("`iwas_XX'" == "1"){
	//local controls "`controls' D_XX"
//replace to_drop_XX = (to_drop_XX==.|`IV_var_XX'==.)
}
//Handle missing values iii)END
//drop if to_drop_XX


//****************************If there is gap
gen tsfilled_XX = 0

xtset ID_XX T_OG_XX
tsfill, full
replace tsfilled_XX = 1 if tsfilled_XX==.
sum tsfilled_XX

********************************************//

gegen T_XX =  group(T_OG_XX)
//save "Tsfilled.dta"

// Creating the weights variable. //weights OPTION //CLUSTER OPTION 


if("`weights'"==""){
gen weights_XX  = 1
gen weights_cXX = 1 
}
else{
gen weights_XX = `weights'
if ("`cluster'"!=""){
	bysort `cluster' T_XX : egen weights_cXX = total(weights_XX)
}
else{
	gen weights_cXX = `weights'
}
}

//replace weights_XX=0 if weights_XX==.
//replace weights_cXX=0 if weights_cXX==.

*******************************************************

**# Bookmark #1 Generate the time pairwise dummies
sum T_XX
scalar max_T = r(max)
local max_T = r(max)

if ("`placebo'"!="0"&scalar(max_T)==2){
	di as error "Placebo cannot be estimated with only two periods."
	di as error "The command will then ignore the option placebo."
	local placebo ""
	
}
if (scalar(max_T)<2){
	di as error "Your time variable takes less than two values."
	exit
}

//// Modif Felix: generate upper bound for number of placebos
cap drop fk_deltaD_glob_XX
cap drop fk_Q_XX
xtset ID_XX T_XX
gen fk_deltaD_glob_XX = abs(D.D_XX)

bysort T_XX: gen fk_stayer_pointer_tempXX = (fk_deltaD_glob_XX==0)
bysort T_XX: egen fk_stayers_t_XX = total(fk_stayer_pointer_tempXX)	

bysort T_XX: gen fk_switcher_pointer_tempXX = (fk_deltaD_glob_XX!=0)
bysort T_XX: egen fk_switchers_t_XX = total(fk_switcher_pointer_tempXX)
	
bysort T_XX: gen fk_used_in_t_estimation_XX = fk_switcher_pointer_tempXX&fk_stayers_t_XX>1

bysort T_XX: egen fk_used_period_XX=max(fk_used_in_t_estimation_XX)
sum T_XX if fk_used_period_XX==1
local fk_max_used_period_XX = r(max)

if `placebo'>`=`fk_max_used_period_XX'-2'{
	
	di ""
	di as error "The maximum number of placebos that can be computed is `=`fk_max_used_period_XX'-2'."
	di as input _continue ""
	
	local placebo = `=`fk_max_used_period_XX'-2'
}


**# Bookmark #2 AGGREGATION TO OBTAIN delta_1, delta_2, and delta_3 and their variances

**# Bookmark #2bis Call the program for each time pairwise dummy
//// BYQUANTILE OPTIONS
local by_quantile = 1
if (`by_fd'>1)  local by_quantile = `by_fd'
if (`by_baseline'>1) local by_quantile = `by_baseline'

if (`by_quantile'>1){
	
	if (`as_XX'==1| `was_XX'==1){
		
	//1. Generate the FD of the treatment / or instrument if specified
	cap drop deltaD_glob_XX
	cap drop Q_XX
	xtset ID_XX T_XX
	gen deltaD_glob_XX = abs(D.D_XX)
	
	//// Consider only switchers that will be used in at least one pair of periods.
	bysort T_XX: gen stayer_pointer_tempXX = (deltaD_glob_XX==0)
	bysort T_XX: egen stayers_t_XX = total(stayer_pointer_tempXX)	

	bysort T_XX: gen switcher_pointer_tempXX = (deltaD_glob_XX!=0)
	bysort T_XX: egen switchers_t_XX = total(switcher_pointer_tempXX)
	
	bysort T_XX:  gen used_in_t_estimation_XX = switcher_pointer_tempXX&stayers_t_XX>1
	xtset ID_XX T_XX
	drop deltaD_glob_XX
	gen deltaD_glob_XX = abs(D.D_XX) if used_in_t_estimation_XX
	
	//if (`by_fd'>) twoway kdensity deltaD_glob_XX, xtitle("DeltaD") ytitle("Density")
	//else 		  twoway kdensity D_XX, xtitle("D") ytitle("Density")
	
	//2. Generate the the quantiles
	if (`by_fd'>1)        xtile Q_XX = deltaD_glob_XX if deltaD_glob_XX!=0, n(`by_quantile') 
	if (`by_baseline'>1){
		replace deltaD_glob_XX = F.deltaD_glob_XX // To make sure we compute the distribution of D_{T-1} among switchers.
		xtile Q_XX = D_XX if deltaD_glob_XX!=0, n(`by_quantile')  
	}
	// 3. The intervals 
	if (`by_fd'>1)        _pctile deltaD_glob_XX      if deltaD_glob_XX!=0, n(`by_quantile')
	if (`by_baseline'>1)  _pctile D_XX                if deltaD_glob_XX!=0, n(`by_quantile') 
	
	forvalues q=1/`by_quantile'{
		scalar r`q'_rr = round(r(r`q'), .001)
	}
    if (`by_fd'>1)           sum deltaD_glob_XX if deltaD_glob_XX!=0
	if (`by_baseline'>1)     sum D_XX           if deltaD_glob_XX!=0 
	
	scalar r0_rr = round(r(min), .001)
	scalar r`by_quantile'_rr = round(r(max), .001)
	
	}
	
	if (`iwas_XX'==1){
		
	//1. Generate the FD of the treatment / or instrument if specified
	cap drop deltaZ_glob_XX
	cap drop QZ_XX
	xtset ID_XX T_XX
	gen deltaZ_glob_XX = abs(D.`IV_var_XX')
	
	
	//2. Generate the the quantiles
	if (`by_fd'>1)  xtile QZ_XX = deltaZ_glob_XX     if deltaZ_glob_XX!=0, n(`by_quantile') 
	if (`by_baseline'>1)   xtile QZ_XX = `IV_var_XX' if deltaZ_glob_XX!=0, n(`by_quantile') 	
		
	// 3. The intervals 
	if (`by_fd'>1)	        _pctile deltaZ_glob_XX if deltaZ_glob_XX!=0, n(`by_quantile')
	if (`by_baseline'>1)	_pctile `IV_var_XX'    if deltaZ_glob_XX!=0, n(`by_quantile')
	
	forvalues q=1/`by_quantile'{
		scalar r`q'_Zrr = round(r(r`q'), .001)
	}
	    if (`by_fd'>1)         sum deltaZ_glob_XX if deltaZ_glob_XX!=0
		if (`by_baseline'>1)   sum `IV_var_XX'    if deltaZ_glob_XX!=0
	scalar r0_Zrr = round(r(min), .001)
	scalar r`by_quantile'_Zrr = round(r(max), .001)
	
	}
	
		/************************
		Making the program byable
		*************************/
		if _by() { // _byvars
		//Check if there is at least one variable in the by option that is not time varying.
		foreach vars in `_byvars'{
			xtset ID_XX T_OG_XX
			xtsum `vars'
			if (`r(sd_w)'!=0&`r(sd_w)'!=.){
				if _byindex() ==1 {
				display as error "The variable `vars' specified in the option bysort is time-varying"
				display as error "The bysort option only allows time-invariant variables."
				exit
				}
				else{
					exit
				}
			}
		}
		//Store the number of levels
		sort `_byvars'
		cap drop by_vars_XX
		egen by_vars_XX = group(`_byvars'), label
		levelsof  by_vars_XX
		local nb_level_by_XX = r(r)
		//Applying the by
		cap drop to_use_XX
		gen to_use_XX = 1
		replace to_use_XX = 0 if `_byindex' != _byindex()
		drop if to_use_XX==0
		}
}
else{
	cap drop Q_XX
	gen Q_XX = 1

	/************************
	Making the program byable
	*************************/
	if _by() { // _byvars
	//Check if there is at least one variable in the by option that is not time varying.
	foreach vars in `_byvars'{
		xtset ID_XX T_OG_XX
		xtsum `vars'
		if (`r(sd_w)'!=0&`r(sd_w)'!=.){
			if _byindex() ==1 {
			display as error "The variable `vars' specified in the option bysort is time-varying"
			display as error "The bysort option only allows time-invariant variables."
			exit
			}
			else{
				exit
			}
		}
	}
	//Store the number of levels
	sort `_byvars'
	cap drop by_vars_XX
	egen by_vars_XX = group(`_byvars'), label
	levelsof  by_vars_XX
	local nb_level_by_XX = r(r)
	//Applying the by
	cap drop to_use_XX
	gen to_use_XX = 1
	replace to_use_XX = 0 if `_byindex' != _byindex()
	drop if to_use_XX==0
	}
}
}

/*****************************************************************************************
MAKE THE PROGRAM COMPATIBLE WITH ESTOUT: STEP 1
****************************************************************************************/
//b of the effects
matrix b = J(1,`=`by_quantile'*`total_estimator'',0)
matrix V = J(`=`by_quantile'*`total_estimator'', `=`by_quantile'*`total_estimator'', 0)
if ("`placebo'"!="0"){
	matrix V = J(2*(`=`by_quantile'*`total_estimator''), 2*(`=`by_quantile'*`total_estimator''), 0)
	matrix b = J(1,2*`=`by_quantile'*`total_estimator'',0)
	
	matrix b_placebo = J(1,`=`by_quantile'*`total_estimator'',0)
}

mata : b = st_matrix("b")
mata : V = st_matrix("V")

local colnames
local rownames 

forvalues q=1/`by_quantile'{
	
	//Keep a dataset with all the IDs: this is very helpful for the computation of tha aggregated influence function (Doulo: Think about the missing values -unbalanced panel- to remember why I prefered this approach over the matrix approach.)
quietly{
	//preserve 1
	tempfile OG_dataPath1
	save "`OG_dataPath1'.dta", replace
	
		duplicates drop ID_XX, force
		keep ID_XX 
		tempfile data_1XX
		save "`data_1XX'.dta", replace
		glob data_1XX = "`data_1XX'"
		
	use "`OG_dataPath1'.dta", clear
	//restore 1
}	
*********************************************************************** if (`by_quantile'>1){ //I will use this to label the graph
//Note: the subprogram return_label is defined at the end of the code, and is used when the user specifies estout along with the option by_fd or by_baseline. Look at the dofile "different options.do" (in section estout + by_fd option) to see how I use it.
if (`by_quantile'>1){
di as input "{hline 80}"
return_label, by_quantile(`by_quantile') by_fd(`by_fd') q(`q') as(`as_XX') was(`was_XX') iwas(`iwas_XX') display_message
di as input "{hline 80}"
}
***********************************************************************
		
quietly{
	//Main variable: To be used after in pairwise when I will start optimizing the code: 
cap drop St_XX
cap drop deltaDt_XX
cap drop Sbist_XX
cap drop StPlus_XX
cap drop StMinus_XX
cap drop S0bist_XX


xtset ID_XX T_XX
gen deltaDt_XX = D.D_XX
gen Sbist_XX   = deltaDt_XX!=0 if deltaDt_XX!=.
gen S0bist_XX  = 1-Sbist_XX
gen StPlus_XX  = deltaDt_XX>0  if  deltaDt_XX!=.
gen StMinus_XX = deltaDt_XX<0  if  deltaDt_XX!=.

if ("`iwas_XX'"=="1"){
cap drop SIt_XX
cap drop deltaZt_XX
cap drop SIbist_XX 
cap drop SItPlus_XX
cap drop SItMinus_XX
cap drop SI0bist_XX

gen deltaZt_XX = D.`IV_var_XX'
gen SIbist_XX   = deltaZt_XX!=0 if deltaZt_XX!=.
gen SI0bist_XX  = 1-SIbist_XX
gen SItPlus_XX  = deltaZt_XX>0  if  deltaZt_XX!=.
gen SItMinus_XX = deltaZt_XX<0  if  deltaZt_XX!=.
}

//This part here is to ensure that twfe is an in the same sample as the estimators. When requested by user.
if ("`same_sample'"!=""){
cap drop yeart_used_XX
cap drop types_yeart_XX
cap drop nb_stayers_yeart_XX
if ("`iwas_XX'"=="1"){
	bysort T_XX : egen types_yeart_XX = sd(SI0bist_XX)
	bysort T_XX : egen nb_stayers_yeart_XX = total(SI0bist_XX)
}
else{
	bysort T_XX : egen types_yeart_XX = sd(S0bist_XX)
	bysort T_XX : egen nb_stayers_yeart_XX = total(S0bist_XX)
}

gen yeart_used_XX = (types_yeart_XX!=0)&(nb_stayers_yeart_XX>=2)
egen tmp_year_time_XX = max(yeart_used_XX) if T_XX<=2
replace yeart_used_XX = tmp_year_time_XX if T_XX == 1
drop tmp_year_time_XX
local if_twfe = "if yeart_used_XX==1"
}
else{
	local if_twfe
}


/*******************************************************************************
CROSS-VALIDATION TO SELECT THE OPTIMAL ORDER : Only with WAS
*******************************************************************************/
if ("`cross_validation'"!=""&`was_XX'==1){
_dots 0, title(`algorithm'(`kfolds'): Cross validation running) reps(`max_k')

//Need to store the suboptions in cross_validation
parse_select_cv_suboptions, `cross_validation'
local cross_validation_logit = "algorithm(kfolds) tolerance(`s(tolerance)') max_k(`s(max_k)') seed(`s(seed)') kfolds(`s(kfolds)')"

// Modif Felix: shut down anything in algo() that is not kfolds
if "`s(algorithm)'"!="kfolds"{
	di as error "The option algo() only allows kfolds as input."
	exit
}


cap drop T_XX_FE_* 
tab T_XX, gen(T_XX_FE_)

cap drop St_XX
cap drop deltaDt_XX
cap drop Sbist_XX // Modif Felix: spelling error corrected
cap drop StPlus_XX
cap drop StMinus_XX
cap drop S0bist_XX

gen deltaDt_XX = D.D_XX
gen Sbist_XX   = deltaDt_XX!=0 if deltaDt_XX!=.
gen S0bist_XX  = 1-Sbist_XX
gen StPlus_XX  = deltaDt_XX>0  if  deltaDt_XX!=.
gen StMinus_XX = deltaDt_XX<0  if  deltaDt_XX!=.

//1. Regression E(Y_t-Y_{t-1}|D_{t-1}) = \sum_{s=2}^{T}1{t=s}PD^{k}_{s-1} with PD^{k}_{s-1} = a_0 + a_1D{s-1} + ... + a_kD_{s-1}^k

	cap drop deltaYt_XX
	xtset ID_XX T_XX
	gen deltaYt_XX = D.Y_XX

//RUN THE CROSS-VALIDATION command
cross_validation deltaYt_XX if Sbist_XX==0 , `cross_validation' 

local reg_order  = `s(chosen_order)'
//di as error "test chosen reg_order  = `reg_order'"

//2. Logit P(S_{t}=0|D_{t-1}) = LOGIT[\sum_{s=2}^{T}1{t=s}PD^{k}_{s-1} with PD^{k}_{s-1} = a_0 + a_1D{s-1} + ... + a_kD_{s-1}^k]


//RUN THE CROSS-VALIDATION command
//di as error "`cross_validation_logit'"
//save "data_cv.dta", replace
cross_validation S0bist_XX , `cross_validation_logit' model(logit)

if (`s(set_chosen_order_linear)' == 1) local logit_bis_order  = `reg_order'
else local logit_bis_order  = `s(chosen_order)'
//di as error "test chosen logit_bis_order  = `logit_bis_order'"

count if StPlus_XX==1
if (r(N)>0){
cross_validation StPlus_XX , `cross_validation_logit' model(logit)
if (`s(set_chosen_order_linear)' == 1) local logit_Plus_order  = `reg_order'
else local logit_Plus_order  = `s(chosen_order)'
//di as error "test chosen logit_Plus_order  = `logit_Plus_order'"
}
count if StMinus_XX==1
if (r(N)>0){
cross_validation StMinus_XX , `cross_validation_logit' model(logit)
if (`s(set_chosen_order_linear)' == 1) local logit_Minus_order  = `reg_order'
else local logit_Minus_order  = `s(chosen_order)'
//di as error "test chosen logit_Minus_order  = `logit_Minus_order'"
}
}
else{
	local reg_order         = `order'
	local logit_bis_order   = `order'
	local logit_Plus_order  = `order'
	local logit_Minus_order = `order'
}
		//preserve quantile
	tempfile OG_dataPathq
	save "`OG_dataPathq'.dta", replace
	

		//Some initialisations for the aggregation
	scalar PS_sum_XX           = 0
	scalar delta1_1XX          = 0

	scalar EabsdeltaD_sum_XX   = 0
	scalar delta2_1XX          = 0

	scalar denom_deltaIV_sum_XX   = 0
	scalar delta3_1XX             = 0 

	scalar N_Switchers2_1XX    = 0
	scalar Nstayers2_1XX       = 0

	scalar N_Switchers1_1XX    = 0
	scalar Nstayers1_1XX       = 0

	scalar N_Switchers3_1XX    = 0
	scalar Nstayers3_1XX       = 0

	scalar N_drop_noextra_XX   = 0 
	scalar N_drop_c_noextra_XX = 0 


//Call the command
forvalues p = 2/`=max_T'{
	
	//i) Calling the command for each pair of time periods
	did_multiplegt_stat_pairwise Y_XX ID_XX T_XX D_XX `IV_var_XX' `if' `in' , estimator(`estimator') or(`order') `noextrapolation' weights(weights_XX) switchers(`switchers') pairwise(`p') data_1XX($data_1XX) as(`as_XX') was(`was_XX') iwas(`iwas_XX') estimation_method(`estimation_method') `exact_match' cluster(`cluster') quantile(`q') by_fd(`by_fd') by_baseline(`by_baseline') other_treatments(`other_treatments') controls(`controls') reg_order(`reg_order') logit_bis_order(`logit_bis_order')  logit_Plus_order(`logit_Plus_order') logit_Minus_order(`logit_Minus_order') cross_validation(`cross_validation')

	//i) Aggregation as the loop goes
	
	//as
	if (`as_XX' == 1){
		if (scalar(delta1_`p'XX)!=.){
		scalar delta1_1XX = scalar(delta1_1XX) + scalar(P_`p'XX)*scalar(delta1_`p'XX)
		
		//Numbers of switchers and stayers: Note that if the estimation is not feasible we do not add the number of switchers/stayers in the total number
		scalar N_Switchers1_1XX = scalar(N_Switchers1_1XX) + N_Switchers1_`p'XX
		scalar Nstayers1_1XX    = scalar(Nstayers1_1XX) + Nstayers1_`p'XX
		}
	}
	
	//was
	if (`was_XX' == 1){
		if (scalar(delta2_`p'XX)!=.){
		scalar delta2_1XX = scalar(delta2_1XX) + scalar(EabsdeltaD_`p'XX)*scalar(delta2_`p'XX)
		
		//Numbers of switchers and stayers: Note that if the estimation is not feasible we do NOT add the number of switchers/stayers in the total number
		scalar N_Switchers2_1XX = scalar(N_Switchers2_1XX) + N_Switchers2_`p'XX
		scalar Nstayers2_1XX    = scalar(Nstayers2_1XX) + Nstayers2_`p'XX
		}
	}
	
	//iwas
	if (`iwas_XX' == 1){
		if (scalar(delta3_`p'XX)!=.){
		scalar delta3_1XX = scalar(delta3_1XX) + scalar(denom_deltaIV`p'XX)*scalar(delta3_`p'XX)
	
		//Numbers of switchers and stayers: Note that if the estimation is not feasible we do NOT add the number of switchers/stayers in the total number
		scalar N_Switchers3_1XX = scalar(N_Switchers3_1XX) + N_Switchers3_`p'XX
		scalar Nstayers3_1XX    = scalar(Nstayers3_1XX) + Nstayers3_`p'XX
		}
	}
	
} //END OF THE LOOP effects

    //iii) Compute the aggregated estimators (Effects and Placebos)
	
	//as
	if (`as_XX' == 1){
		scalar delta1_1XX = scalar(delta1_1XX)/scalar(PS_sum_XX)
	}
	
	//was
	if (`was_XX' == 1){
		scalar delta2_1XX = scalar(delta2_1XX)/scalar(EabsdeltaD_sum_XX)
	}
	
	//iwas
	if (`iwas_XX' == 1){
		scalar delta3_1XX = scalar(delta3_1XX)/scalar(denom_deltaIV_sum_XX)
	}
	//iv) Compute the influence functions
	
	//preserve 2
	tempfile OG_dataPath2
	save "`OG_dataPath2'.dta", replace
	
	use "$data_1XX.dta", clear
	
	//save TestOfPhis.dta, replace //store the datasets with all the IFs just for the sake of checking.
	
	//Effects
	local Phi1_ts_XX 
	local Phi2_ts_XX 
	local Phi3_ts_XX
	
	forvalues p = 2/`=max_T'{
		
	//as: Phi1^{T>2}
	if (`as_XX' == 1){
		//Effects
		replace Phi1_`p'XX = [scalar(P_`p'XX)*Phi1_`p'XX + (scalar(delta1_`p'XX) - scalar(delta1_1XX))*(S_`p'XX - scalar(P_`p'XX))]/scalar(PS_sum_XX)
		local Phi1_ts_XX `Phi1_ts_XX' Phi1_`p'XX
	}
	
	//was: Phi2^{T>2}
	if (`was_XX' == 1){
		
		//Effects
		replace Phi2_`p'XX = [scalar(EabsdeltaD_`p'XX)*Phi2_`p'XX + (scalar(delta2_`p'XX) - scalar(delta2_1XX))*(absdeltaD_`p'XX - scalar(EabsdeltaD_`p'XX))]/scalar(EabsdeltaD_sum_XX)
		
		////tEST
		sum  Phi2_`p'XX
		//scalar mean_IF2bis_`p' = r(mean)
		////
		local Phi2_ts_XX `Phi2_ts_XX' Phi2_`p'XX
	}
		
	//iwas: Phi3^{T>2}
	if (`iwas_XX' == 1){
		
		//Effects
		replace Phi3_`p'XX = [scalar(denom_deltaIV`p'XX)*Phi3_`p'XX + (scalar(delta3_`p'XX) - scalar(delta3_1XX))*(innerSumIV_denom_`p'XX - scalar(denom_deltaIV`p'XX))]/scalar(denom_deltaIV_sum_XX)
		local Phi3_ts_XX `Phi3_ts_XX' Phi3_`p'XX
	}
	} //END OF THE LOOP
if ("`cluster'"!=""){ 
	//Compute E(N_c)
bysort `cluster': gen N_c_XX = _N if _n==1
sum N_c_XX
scalar N_bar_c_XX = r(mean)
//di as error N_bar_c_XX
}
	//as
	if (`as_XX' == 1){
		
		//Effect
		cap drop Phi1_XX
		cap drop not_to_use1_XX
		//di as error "`Phi1_ts_XX'"
		gegen Phi1_XX = rowtotal(`Phi1_ts_XX')
		gegen not_to_use1_XX = rownonmiss(`Phi1_ts_XX') //Count the number of nonmiss, then if not_to_use_XX==0, we set Phi1_XX==.
		replace Phi1_XX=. if not_to_use1_XX==0
		
		sum Phi1_XX 
		//scalar mean_IF1 = r(mean) //for test 
		
		if ("`cluster'"!=""){ // Clustering the variance //CLUSTER OPTION
		cap drop Phi1_cXX
		bysort `cluster': gegen Phi1_cXX = total(Phi1_XX)
		bysort `cluster': replace Phi1_cXX=. if _n!=1
		replace Phi1_cXX = Phi1_cXX/scalar(N_bar_c_XX)
		
			if ("`weights'"!=""){
			sum Phi1_cXX  // use the weights of the cluster, which is the total of the weightss of groups in that cluster.
			}
			else{
			sum Phi1_cXX 
			}

		}
		
		scalar sd_delta1_1XX = r(sd)/sqrt(r(sum_w)) //Doulo: In case of unbalanced panel, some units do not contribute to the influence function at some dates, what about the N, in the asymptotic normal distribution?
		
		scalar LB1_1XX = delta1_1XX - 1.96*sd_delta1_1XX
		scalar UB1_1XX = delta1_1XX + 1.96*sd_delta1_1XX
	}
	
	//was
	if (`was_XX' == 1){
		
		//Effect
		cap drop Phi2_XX
		cap drop not_to_use2_XX
		gegen Phi2_XX = rowtotal(`Phi2_ts_XX')
        gegen not_to_use2_XX = rownonmiss(`Phi2_ts_XX') //Count the number of nonmiss, then if not_to_use_XX==0, we set Phi2_XX==.
		replace Phi2_XX=. if not_to_use2_XX==0
		
		sum Phi2_XX 
		//scalar mean_IF2 = r(mean) //for test
		scalar N_var = r(sum_w)
		if ("`cluster'"!=""){ // Clustering the variance //CLUSTER OPTION
		cap drop Phi2_cXX
		bysort `cluster': gegen Phi2_cXX = total(Phi2_XX)
		bysort `cluster': replace Phi2_cXX=. if _n!=1
		replace Phi2_cXX = Phi2_cXX/scalar(N_bar_c_XX)
		
				if ("`weights'"!=""){
				sum Phi2_cXX // use the weights of the cluster, which is the total of the weightss of groups in that cluster.
				}
				else{
				sum Phi2_cXX 
				/*di as error "hh" r(sd)
				di as error r(sum_w)
				di as error scalar(N_var)*/
				}
		}
		
		scalar sd_delta2_1XX = r(sd)/sqrt(r(sum_w)) 
		
		scalar LB2_1XX = delta2_1XX - 1.96*sd_delta2_1XX
		scalar UB2_1XX = delta2_1XX + 1.96*sd_delta2_1XX
		
	}

	//iwas
	if (`iwas_XX' == 1){
		
		//Effect
		cap drop Phi3_XX
		cap drop not_to_use3_XX
		gegen Phi3_XX = rowtotal(`Phi3_ts_XX')
        gegen not_to_use3_XX = rownonmiss(`Phi3_ts_XX') //Count the number of nonmiss, then if not_to_use_XX==0, we set Phi2_XX==.
		replace Phi3_XX=. if not_to_use3_XX==0
		
		sum Phi3_XX 
		scalar mean_IF3 = r(mean) //for test
		
		if ("`cluster'"!=""){ // Clustering the variance //CLUSTER OPTION
		cap drop Phi3_cXX
		bysort `cluster': gegen Phi3_cXX = total(Phi3_XX)
		bysort `cluster': replace Phi3_cXX=. if _n!=1
		replace Phi3_XX = Phi3_XX/scalar(N_bar_c_XX)		
				if ("`weights'"!=""){
				sum Phi3_cXX // use the weights of the cluster, which is the total of the weightss of groups in that cluster.
				}
				else{
				sum Phi3_cXX 
				}
		}
		
		scalar sd_delta3_1XX = r(sd)/sqrt(r(sum_w))
		
		scalar LB3_1XX = delta3_1XX - 1.96*sd_delta3_1XX
		scalar UB3_1XX = delta3_1XX + 1.96*sd_delta3_1XX
	}

	
/******************************************************************************
TESTING THE DIFFERENCE BETWEEN AS AND WAS IF REQUESTED

Note: Let UPhi1 and UPhi2 be the non-demeaned versions of Phi1 and Phi2
Testing H_0: as = was is equivalent to testing H0: meanUPhi1 = meanUPhi2
H0: meanUPhi1 = meanUPhi2 <=> H0: mean(UPhi1 - UPhi2) = 0.
Under H_0, mean(UPhi1 - UPhi2) = 0 <=> mean(Phi1 - Phi2) = 0.
Then we can just test whether the variable  diff_Phi1_2_XX = Phi1_XX - Phi2_XX
is mean-zero or not. This is also equivalent to run reg diff_Phi1_2_XX.
*******************************************************************************/
if ("`as_vs_was'"!=""&`a_vs_w'==2){
	
	scalar diff_delta1_2_XX = scalar(delta1_1XX) - scalar(delta2_1XX)
	cap drop diff_Phi1_2_XX 
	gen  diff_Phi1_2_XX = Phi1_XX - Phi2_XX
	sum diff_Phi1_2_XX
    scalar sd_diff_Phi1_2_XX = r(sd)
	scalar tstat_XX = scalar(diff_delta1_2_XX)*sqrt(r(N))/sd_diff_Phi1_2_XX
	scalar pval_as_vs_was = 2*(1-normal(abs(tstat_XX)))
	
	matrix as_vs_was_mat = J(1,6,.)
	matrix as_vs_was_mat[1,1] = scalar(diff_delta1_2_XX)
	matrix as_vs_was_mat[1,2] = scalar(sd_diff_Phi1_2_XX)
	matrix as_vs_was_mat[1,3] = scalar(diff_delta1_2_XX) - 1.96*scalar(sd_diff_Phi1_2_XX)/sqrt(r(N))
	matrix as_vs_was_mat[1,4] = scalar(diff_delta1_2_XX) + 1.96*scalar(sd_diff_Phi1_2_XX)/sqrt(r(N))
	matrix as_vs_was_mat[1,5] = scalar(pval_as_vs_was)
	matrix as_vs_was_mat[1,6] = scalar(tstat_XX)
	
	matrix colnames as_vs_was_mat = "Diff." "SE" "LB CI" "UB CI" "pval." "t"
    matrix rownames as_vs_was_mat = "AS-WAS"
	
	// matrix as_vs_was_mat`q' = as_vs_was_mat // Modif Felix: store as_vs_was matrix in ereturns, however still need to adapt for by etc.
	//local myN = r(N)
}

/*****************************************************************************************************/
	//Placebos' versions ROUTINE START
/*****************************************************************************************************/
	if("`placebo'"!="0"){
	
	forvalues placebo_index = 1/`placebo'{
cap macro drop data_1plaXX 
		
use "`OG_dataPath1'.dta", clear
	
		duplicates drop ID_XX, force
		keep ID_XX 
		tempfile data_1plaXX
		save "`data_1plaXX'.dta", replace
		glob data_1plaXX = "`data_1plaXX'"

			//restore quantile
use "`OG_dataPathq'.dta", clear

			//preserve 2
	tempfile OG_dataPath2
	save "`OG_dataPath2'.dta", replace
	
		//INTIALIZATION
	scalar PS_sum_plaXX           = 0
	scalar delta1_1plaXX          = 0

	scalar EabsdeltaD_sum_plaXX   = 0
	scalar delta2_1plaXX          = 0

	scalar denom_deltaIV_sum_plaXX   = 0
	scalar delta3_1plaXX             = 0 

	scalar N_Switchers2_1plaXX    = 0
	scalar Nstayers2_1plaXX       = 0

	scalar N_Switchers1_1plaXX    = 0
	scalar Nstayers1_1plaXX       = 0

	scalar N_Switchers3_1plaXX    = 0
	scalar Nstayers3_1plaXX       = 0

	scalar N_drop_noextra_plaXX   = 0 
	scalar N_drop_c_noextra_plaXX = 0 
	
			//COMPUATTION OF THE PLACEBOS
			
	forvalues p = `=2+`placebo_index''/`=max_T'{
	
	//i) Calling the command for each pair of time periods
	did_multiplegt_stat_pairwise Y_XX ID_XX T_XX D_XX `IV_var_XX' `if' `in' , estimator(`estimator') or(`order') `noextrapolation' weights(weights_XX) switchers(`switchers') pairwise(`p') data_1XX(${data_1plaXX}) as(`as_XX') was(`was_XX') iwas(`iwas_XX') estimation_method(`estimation_method') placebo(`placebo_index') `exact_match' cluster(`cluster')  quantile(`q') by_fd(`by_fd') by_baseline(`by_baseline') other_treatments(`other_treatments') controls(`controls') reg_order(`reg_order') logit_bis_order(`logit_bis_order')  logit_Plus_order(`logit_Plus_order') logit_Minus_order(`logit_Minus_order') cross_validation(`cross_validation')

	//i) Aggregation as the loop goes
	
	//as
	if (`as_XX' == 1){
		if (scalar(delta1_`p'plaXX)!=.){
		scalar delta1_1plaXX = scalar(delta1_1plaXX) + scalar(P_`p'plaXX)*scalar(delta1_`p'plaXX)
		
		//Numbers of switchers and stayers: Note that if the estimation is not feasible we do not add the number of switchers/stayers in the total number
		scalar N_Switchers1_1plaXX = scalar(N_Switchers1_1plaXX) + scalar(N_Switchers1_`p'plaXX)
		scalar Nstayers1_1plaXX    = scalar(Nstayers1_1plaXX) + scalar(Nstayers1_`p'plaXX)
		}
	}
	
	//was
	if (`was_XX' == 1){
		if (scalar(delta2_`p'plaXX)!=.){
		scalar delta2_1plaXX = scalar(delta2_1plaXX) + scalar(EabsdeltaD_`p'plaXX)*scalar(delta2_`p'plaXX)
		
		//Numbers of switchers and stayers: Note that if the estimation is not feasible we do not add the number of switchers/stayers in the total number
		scalar N_Switchers2_1plaXX = scalar(N_Switchers2_1plaXX) + scalar(N_Switchers2_`p'plaXX)
		scalar Nstayers2_1plaXX    = scalar(Nstayers2_1plaXX) + scalar(Nstayers2_`p'plaXX)
		}
	}
	
	//iwas
	if (`iwas_XX' == 1){
		if (scalar(delta3_`p'plaXX)!=.){
		scalar delta3_1plaXX = scalar(delta3_1plaXX) + scalar(denom_deltaIV`p'plaXX)*scalar(delta3_`p'plaXX)
			
		//Numbers of switchers and stayers: Note that if the estimation is not feasible we do NOT add the number of switchers/stayers in the total number
		scalar N_Switchers3_1plaXX = scalar(N_Switchers3_1plaXX) + scalar(N_Switchers3_`p'plaXX)
		scalar Nstayers3_1plaXX    = scalar(Nstayers3_1plaXX) + scalar(Nstayers3_`p'plaXX)
		}
	}
	
} //END OF THE LOOP PLACEBO
	
	
    //Compute the aggregated estimators (Effects and Placebos)
	
	//as
	if (`as_XX' == 1)  {
		scalar delta1_1plaXX = scalar(delta1_1plaXX)/scalar(PS_sum_plaXX)
	}
	
	//was
	if (`was_XX' == 1) {
		scalar delta2_1plaXX = scalar(delta2_1plaXX)/scalar(EabsdeltaD_sum_plaXX)
	}
	
	//iwas
	if (`iwas_XX' == 1){
		scalar delta3_1plaXX = scalar(delta3_1plaXX)/scalar(denom_deltaIV_sum_plaXX)
		
	}
	
	// Compute the influence functions
	
	use "$data_1plaXX.dta", clear
	
	local Phi1_ts_plaXX 
	local Phi2_ts_plaXX 
	local Phi3_ts_plaXX 	
	
	//// aggregating IFs
	forvalues p = 2/`=max_T'{
		
	//as: Phi1^{T>2}
	if (`as_XX' == 1){
		if (`p'>`=`placebo_index'+1'){
			replace Phi1_`p'plaXX = [scalar(P_`p'plaXX)*Phi1_`p'plaXX + (scalar(delta1_`p'plaXX) - scalar(delta1_1plaXX))*(S_`p'plaXX - scalar(P_`p'plaXX))]/scalar(PS_sum_plaXX)
			local Phi1_ts_plaXX `Phi1_ts_plaXX' Phi1_`p'plaXX
		}
	}
	
	//was: Phi2^{T>2}
	if (`was_XX' == 1){

		if (`p'>`=`placebo_index'+1'){
			replace Phi2_`p'plaXX = [scalar(EabsdeltaD_`p'plaXX)*Phi2_`p'plaXX + (scalar(delta2_`p'plaXX) - scalar(delta2_1plaXX))*(absdeltaD_`p'plaXX - scalar(EabsdeltaD_`p'plaXX))]/scalar(EabsdeltaD_sum_plaXX)
			local Phi2_ts_plaXX `Phi2_ts_plaXX' Phi2_`p'plaXX
		}
	}
		
	//iwas: Phi3^{T>2}
	if (`iwas_XX' == 1){
		
		if (`p'>`=`placebo_index'+1'){
			replace Phi3_`p'plaXX = [scalar(denom_deltaIV`p'plaXX)*Phi3_`p'plaXX + (scalar(delta3_`p'plaXX) - scalar(delta3_1plaXX))*(innerSumIV_denom_`p'plaXX - scalar(denom_deltaIV`p'plaXX))]/scalar(denom_deltaIV_sum_plaXX)
			local Phi3_ts_plaXX `Phi3_ts_plaXX' Phi3_`p'plaXX
		}
	}
	} //END OF THE LOOP	
	
	//// Final IFs and SE
		//as
	if (`as_XX' == 1){
			cap drop Phi1_plaXX
			cap drop not_to_use1_plaXX
			gegen Phi1_plaXX = rowtotal(`Phi1_ts_plaXX')
			gegen not_to_use1_plaXX = rownonmiss(`Phi1_ts_plaXX') 
			replace Phi1_plaXX=. if not_to_use1_plaXX==0
			
			sum Phi1_plaXX 
			//scalar mean_IF1pla = r(mean) //for test 
			
			if ("`cluster'"!=""){ // Clustering the variance //CLUSTER OPTION
			cap drop Phi1_cXX
			gegen Phi1_placXX = total(Phi1_plaXX) , by(`cluster')
			bysort `cluster': replace Phi1_placXX=. if _n!=1
		    replace Phi1_placXX = Phi1_placXX/scalar(N_bar_c_XX)
				if ("`weights'"!=""){
				sum Phi1_placXX // use the weights of the cluster, which is the total of the weightss of groups in that cluster.
				}
				else{
				sum Phi1_placXX 
				}
			}
		
			scalar sd_delta1_1plaXX = r(sd)/sqrt(r(sum_w)) 
			
			scalar LB1_1plaXX = delta1_1plaXX - 1.96*sd_delta1_1plaXX
			scalar UB1_1plaXX = delta1_1plaXX + 1.96*sd_delta1_1plaXX	
	}
		//was	
	if (`was_XX' == 1){
				cap drop Phi2_plaXX
			cap drop not_to_use2_plaXX
			gegen Phi2_plaXX = rowtotal(`Phi2_ts_plaXX')
			gegen not_to_use2_plaXX = rownonmiss(`Phi2_ts_plaXX') 
			replace Phi2_plaXX=. if not_to_use2_plaXX==0
			
			sum Phi2_plaXX 
			//scalar mean_IF2pla = r(mean) //for test 
			
		if ("`cluster'"!=""){ // Clustering the variance //CLUSTER OPTION
		cap drop Phi2_placXX
		bysort `cluster': gegen Phi2_placXX = total(Phi2_plaXX)
		bysort `cluster': replace Phi2_placXX=. if _n!=1
		replace Phi2_placXX = Phi2_placXX/scalar(N_bar_c_XX)
				if ("`weights'"!=""){
				sum Phi2_placXX // use the weights of the cluster, which is the total of the weightss of groups in that cluster.
				}
				else{
				sum Phi2_placXX 
				}
		}
		
			scalar sd_delta2_1plaXX = r(sd)/sqrt(r(sum_w)) 
			
			scalar LB2_1plaXX = delta2_1plaXX - 1.96*sd_delta2_1plaXX
			scalar UB2_1plaXX = delta2_1plaXX + 1.96*sd_delta2_1plaXX
	}
	
		//iwas
	if (`iwas_XX' == 1){
		cap drop Phi3_plaXX
			cap drop not_to_use3_plaXX
			gegen Phi3_plaXX = rowtotal(`Phi3_ts_plaXX')
			gegen not_to_use3_plaXX = rownonmiss(`Phi3_ts_plaXX') 
			replace Phi3_plaXX=. if not_to_use3_plaXX==0
			
			sum Phi3_plaXX 
			scalar mean_IF3pla = r(mean) //for test 
			
		if ("`cluster'"!=""){ // Clustering the variance //CLUSTER OPTION
		cap drop Phi3_placXX
		bysort `cluster': egen Phi3_placXX = total(Phi3_plaXX)
		bysort `cluster': replace Phi3_placXX=. if _n!=1
		replace Phi3_placXX = Phi3_placXX/scalar(N_bar_c_XX)		
				if ("`weights'"!=""){
				sum Phi3_placXX // use the weights of the cluster, which is the total of the weightss of groups in that cluster.
				}
				else{
				sum Phi3_placXX 
				}
		}
			scalar sd_delta3_1plaXX = r(sd)/sqrt(r(sum_w)) 
			
			scalar LB3_1plaXX = delta3_1plaXX - 1.96*sd_delta3_1plaXX
			scalar UB3_1plaXX = delta3_1plaXX + 1.96*sd_delta3_1plaXX
		}
	
	
	//Fill in the matrices
	scalar nb_rows_plaXX = 3*(scalar(max_T))
	local  rownames_plaXX 
	matrix res_mat_plaXX = J(`=nb_rows_plaXX',6,.) 
	
//i. Values
forvalues P = 1/3{
forvalues p = 1/`=max_T'{
	scalar P_XX = `=`P''
	scalar index_XX = (scalar(P_XX)-1)*(scalar(max_T))
	if (`as_XX' == 1&`P'==1){
	
	if ((`p'==1|`p'>`=`placebo_index'+1')){
		
			**********************************************************	
			matrix res_mat_plaXX[`=index_XX' + `p',1] = scalar(delta`P'_`p'plaXX)
			matrix res_mat_plaXX[`=index_XX' + `p',2] = sd_delta`P'_`p'plaXX
			matrix res_mat_plaXX[`=index_XX' + `p',3] = LB`P'_`p'plaXX
			matrix res_mat_plaXX[`=index_XX' + `p',4] = UB`P'_`p'plaXX
			matrix res_mat_plaXX[`=index_XX' + `p',5] = N_Switchers`P'_`p'plaXX
			matrix res_mat_plaXX[`=index_XX' + `p',6] = Nstayers`P'_`p'plaXX
	}
	
	}
	if (`was_XX' == 1&`P'==2){

	if ((`p'==1|`p'>`=`placebo_index'+1')){

			**********************************************************	
			
			matrix res_mat_plaXX[`=index_XX' + `p',1] = scalar(delta`P'_`p'plaXX)
			matrix res_mat_plaXX[`=index_XX' + `p',2] = sd_delta`P'_`p'plaXX
			matrix res_mat_plaXX[`=index_XX' + `p',3] = LB`P'_`p'plaXX
			matrix res_mat_plaXX[`=index_XX' + `p',4] = UB`P'_`p'plaXX
			matrix res_mat_plaXX[`=index_XX' + `p',5] = N_Switchers`P'_`p'plaXX
			matrix res_mat_plaXX[`=index_XX' + `p',6] = Nstayers`P'_`p'plaXX
	}
	
	}
	if (`iwas_XX' == 1&`P'==3){

	if ((`p'==1|`p'>`=`placebo_index'+1')){

			**********************************************************	
			matrix res_mat_plaXX[`=index_XX' + `p',1] = scalar(delta`P'_`p'plaXX)
			matrix res_mat_plaXX[`=index_XX' + `p',2] = sd_delta`P'_`p'plaXX
			matrix res_mat_plaXX[`=index_XX' + `p',3] = LB`P'_`p'plaXX
			matrix res_mat_plaXX[`=index_XX' + `p',4] = UB`P'_`p'plaXX
			matrix res_mat_plaXX[`=index_XX' + `p',5] = N_Switchers`P'_`p'plaXX
			matrix res_mat_plaXX[`=index_XX' + `p',6] = Nstayers`P'_`p'plaXX
	}
	
	}
//rownames
	if (`p'>1){
	local rownames_plaXX `rownames_plaXX' " Placebo_`placebo_index'_`p'"
	}
	else{
		local rownames_plaXX `rownames_plaXX' "Placebo_`placebo_index'"
	}

}
}

//ii. Colnames
matrix colnames res_mat_plaXX= "Estimate" "SE" "LB CI" "UB CI" "Switchers" "Stayers" 

//ii.. Rownames
matrix rownames res_mat_plaXX = `rownames_plaXX'

	
matrix V`placebo_index'res_mat_plaXX = res_mat_plaXX	

	// restore 2
	use  "`OG_dataPath2'.dta", clear	
	
	}
}
/*****************************************************************************************************/
	//Placebos' versions ROUTINE END
/*****************************************************************************************************/

	// restore 2
	use "`OG_dataPath2'.dta", clear
}
if `bootstrap'>0{

/*******************************************************************************
START OF BOOTSTRAPING IF REQUESTED (WITH IV-WAS or with TWFE comparisons).
*******************************************************************************/

if ("`iwas_XX'"=="1") matrix IVeffects_bootstrap = J(`bootstrap', scalar(max_T), .)
if ("`was_XX'"=="1") matrix WASeffects_bootstrap = J(`bootstrap', scalar(max_T), .)
if ("`as_XX'"=="1") matrix ASeffects_bootstrap = J(`bootstrap', scalar(max_T), .)

if ("`placebo'"!="0"){
	if ("`iwas_XX'"=="1") matrix IVplacebos_bootstrap = J(`bootstrap', scalar(max_T), .)
	if ("`was_XX'"=="1") matrix WASplacebos_bootstrap = J(`bootstrap', scalar(max_T), .)
	if ("`as_XX'"=="1") matrix ASplacebos_bootstrap = J(`bootstrap', scalar(max_T), .)	
}

if ("`twfe'"!=""){
matrix bootstrap_order = J(`bootstrap', 1, .)
matrix twfe_bootstrap = J(`bootstrap', 1, .)
}
	//A: Store the point estimates obtained from the whole sample from the estimation above
	forvalues p = 1/`=max_T'{
		cap scalar delta3_`p'OGXX = scalar(delta3_`p'XX)
		cap scalar delta2_`p'OGXX = scalar(delta2_`p'XX)
		cap scalar delta1_`p'OGXX = scalar(delta1_`p'XX)
		
		if ("`placebo'"!="0"&(`p'==1|`p'>`=`placebo'+1')) {
			cap scalar delta3_`p'plaOGXX = scalar(delta3_`p'plaXX)
			cap scalar delta2_`p'plaOGXX = scalar(delta2_`p'plaXX)
			cap scalar delta1_`p'plaOGXX = scalar(delta1_`p'plaXX)
		}
	}
		
	
forvalue i=1/`bootstrap'{
	if ("`iwas_XX'"=="1"){
	scalar bt_delta3_1XX = 0
	scalar bt_delta3_1plaXX = 0
	
	scalar denom_deltaIV_sum_XX = 0
	scalar denom_deltaIV_sum_plaXX = 0
	}
	
	if ("`was_XX'"=="1"){
	scalar bt_delta2_1XX = 0
	scalar bt_delta2_1plaXX = 0
	
	scalar EabsdeltaD_sum_XX = 0
	scalar EabsdeltaD_sum_plaXX = 0
	}
	
	if ("`as_XX'"=="1"){
	scalar bt_delta1_1XX = 0
	scalar bt_delta1_1plaXX = 0
	
	scalar PS_sum_XX = 0
	scalar PS_sum_plaXX = 0
	}
	
	//Progress Bar
	sleep 1
    _dots `i' 0
quietly{	
//i. Set the seed if requested

if `seed'!=0{
set seed `=`seed'+`i''
}

//ii. Besample
cap drop temp_ID_XX
cap drop new_ID_XX
cap drop b_`cluster'XX

if ("`cluster'"==""){
	gen b_XX = ID_XX
}
else{
	gen b_`cluster'XX = `cluster'
}
//// This trick is to avoid the problem of repeated times in panel data.

	bsample, cluster( b_`cluster'XX) idcluster(temp_ID_XX)
	egen new_ID_XX = group(ID_XX temp_ID_XX)
	replace ID_XX = new_ID_XX
	drop new_ID_XX
	//save "bsample.dta", replace
	
if ("`twfe'"!=""){
cap drop T_XX_FE_* // Modif Felix: error when running with bootstrap
tab T_XX, gen(T_XX_FE_)
tab ID_XX, gen (ID_XX_FE_)

if ("`iwas_XX'"=="1") ivreg Y_XX (D_XX = `IV_var_XX') T_XX_FE_* ID_XX_FE_* `if_twfe' 
else                      reg Y_XX D_XX T_XX_FE_* ID_XX_FE_* `if_twfe'

matrix twfe_bootstrap[`i',1] = e(b)[1,1]
matrix bootstrap_order[`i',1] = `i'
}
//save "bsample.dta", replace

//iii. Run the command
	
forvalues p = 2/`=max_T'{	
	// Calling the command for each pair of time periods
	did_multiplegt_stat_pairwise Y_XX ID_XX T_XX D_XX `IV_var_XX' `if' `in' , estimator(`estimator') or(`order') `noextrapolation' weights(weights_XX) switchers(`switchers') pairwise(`p') data_1XX($data_1XX) as(`as_XX') was(`was_XX') iwas(`iwas_XX') estimation_method(`estimation_method') `exact_match' cluster(`cluster') quantile(`q') by_fd(`by_fd') by_baseline(`by_baseline') other_treatments(`other_treatments') controls(`controls')  bootstrap(`bootstrap') reg_order(`reg_order') logit_bis_order(`logit_bis_order')  logit_Plus_order(`logit_Plus_order') logit_Minus_order(`logit_Minus_order') cross_validation(`cross_validation')
	
// Put results into a matrix 
if ("`iwas_XX'"=="1") matrix IVeffects_bootstrap[`i',`p'] = scalar(delta3_`p'XX)
if ("`was_XX'"=="1")  matrix WASeffects_bootstrap[`i',`p'] = scalar(delta2_`p'XX)
if ("`as_XX'"=="1")   matrix ASeffects_bootstrap[`i',`p'] = scalar(delta1_`p'XX)


	//denominators of the aggregated versions
	if (`iwas_XX' == 1){
		if (scalar(delta3_`p'XX)!=.){
			scalar bt_delta3_1XX = scalar(bt_delta3_1XX) + scalar(denom_deltaIV`p'XX)*scalar(delta3_`p'XX)
		}
	}
	if (`was_XX' == 1){
		if (scalar(delta2_`p'XX)!=.){
			scalar bt_delta2_1XX = scalar(bt_delta2_1XX) + scalar(EabsdeltaD_`p'XX)*scalar(delta2_`p'XX)
		}
	}
	if (`as_XX' == 1){
		if (scalar(delta1_`p'XX)!=.){
			scalar bt_delta1_1XX = scalar(bt_delta1_1XX) + scalar(P_`p'XX)*scalar(delta1_`p'XX)
		}
	}
}
	//final aggregations
	if (`iwas_XX' == 1){
			scalar bt_delta3_1XX = scalar(bt_delta3_1XX)/scalar(denom_deltaIV_sum_XX)
	}
	if (`was_XX' == 1){
			scalar bt_delta2_1XX = scalar(bt_delta2_1XX)/scalar(EabsdeltaD_sum_XX)
	}
	if (`as_XX' == 1){
			scalar bt_delta1_1XX = scalar(bt_delta1_1XX)/scalar(PS_sum_XX)
	}
// Put results into a matrix 
cap matrix IVeffects_bootstrap[`i',1]    = scalar(bt_delta3_1XX)
cap matrix WASeffects_bootstrap[`i',1] = scalar(bt_delta2_1XX)
cap matrix ASeffects_bootstrap[`i',1]  = scalar(bt_delta1_1XX)

if ("`placebo'"!="0"){
	forvalues p = `=2+`placebo''/`=max_T'{
	//Store the point estimates obtained from the whole sample from the estimation above
	//scalar delta3_`p'OGplaXX = scalar(delta3_`p'plaXX)
	
		//i) Calling the command for each pair of time periods
		did_multiplegt_stat_pairwise Y_XX ID_XX T_XX D_XX `IV_var_XX' `if' `in' , estimator(`estimator') or(`order') `noextrapolation' weights(weights_XX) switchers(`switchers') pairwise(`p') data_1XX($data_1XX) as(`as_XX') was(`was_XX') iwas(`iwas_XX') estimation_method(`estimation_method') placebo(`placebo') `exact_match' cluster(`cluster')  quantile(`q') by_fd(`by_fd') by_baseline(`by_baseline') other_treatments(`other_treatments') controls(`controls') bootstrap(`bootstrap') reg_order(`reg_order') logit_bis_order(`logit_bis_order')  logit_Plus_order(`logit_Plus_order') logit_Minus_order(`logit_Minus_order') cross_validation(`cross_validation')
				
// Put results into a matrix 
cap matrix IVplacebos_bootstrap[`i',`p'] = scalar(delta3_`p'plaXX)\
cap matrix WASplacebos_bootstrap[`i',`p'] = scalar(delta2_`p'plaXX)
cap matrix ASplacebos_bootstrap[`i',`p'] = scalar(delta1_`p'plaXX)

if (`iwas_XX' == 1){
	if (scalar(delta3_`p'plaXX)!=.){
		scalar bt_delta3_1plaXX = scalar(bt_delta3_1plaXX) + scalar(denom_deltaIV`p'plaXX)*scalar(delta3_`p'plaXX)
		}
}
if (`was_XX' == 1){
	if (scalar(delta2_`p'plaXX)!=.){
		scalar bt_delta2_1plaXX = scalar(bt_delta2_1plaXX) + scalar(EabsdeltaD_`p'plaXX)*scalar(delta2_`p'plaXX)
		}
}
if (`as_XX' == 1){
	if (scalar(delta1_`p'plaXX)!=.){
		scalar bt_delta1_1plaXX = scalar(bt_delta1_1plaXX) + scalar(P_`p'plaXX)*scalar(delta1_`p'plaXX)
		}
}
}

	//final aggregations
	if (`iwas_XX' == 1){
			scalar bt_delta3_1plaXX = scalar(bt_delta3_1plaXX)/scalar(denom_deltaIV_sum_plaXX)
	}	
	if (`was_XX' == 1){
			scalar bt_delta2_1plaXX = scalar(bt_delta2_1plaXX)/scalar(EabsdeltaD_sum_XX)
	}	
	if (`as_XX' == 1){
			scalar bt_delta1_1plaXX = scalar(bt_delta1_1plaXX)/scalar(PS_sum_XX)
	}	
// Put results into a matrix 
cap matrix IVplacebos_bootstrap[`i',1]    = scalar(bt_delta3_1plaXX)
cap matrix WASplacebos_bootstrap[`i',1] = scalar(bt_delta2_1plaXX)
cap matrix ASplacebos_bootstrap[`i',1]  = scalar(bt_delta1_1plaXX)
		
}

use "`OG_dataPathq'.dta", clear
//cap scalars_to_drop //This program is defined below
//Do the reverse of A
//scalar delta3_1XX = scalar(delta3_1XXOG)	
}
}
/*******************************************************************************
END OF BOOTSTRAPING and Restore the original point estimates from estimation using the whole sanmple
*******************************************************************************/
	forvalues p = 1/`=max_T'{
		if (`iwas_XX' == 1){
		scalar delta3_`p'XX = scalar(delta3_`p'OGXX)
		if ("`placebo'"!="0"&(`p'==1|`p'>`=`placebo'+1'))  scalar delta3_`p'plaXX = scalar(delta3_`p'plaOGXX)
		}
		
		if (`was_XX' == 1){
		scalar delta2_`p'XX = scalar(delta2_`p'OGXX)
		if ("`placebo'"!="0"&(`p'==1|`p'>`=`placebo'+1'))  scalar delta2_`p'plaXX = scalar(delta2_`p'plaOGXX)
		}
		
		if (`as_XX' == 1){
		scalar delta1_`p'XX = scalar(delta1_`p'OGXX)
		if ("`placebo'"!="0"&(`p'==1|`p'>`=`placebo'+1'))  scalar delta1_`p'plaXX = scalar(delta1_`p'plaOGXX)
		}
	}

	
di "Completed!"
//Percentile CIs
quietly{
	
if (`iwas_XX' == 1){ //Only bootstrap se if iv-was
clear 
svmat IVeffects_bootstrap	

forvalues p = 1/`=max_T'{
	_pctile IVeffects_bootstrap`p', nq(40)
	scalar LB3_`p'XX = r(r1)
	scalar UB3_`p'XX = r(r39)
	sum IVeffects_bootstrap`p'
	scalar sd_delta3_`p'XX = r(sd)
}
}

if ("`twfe'"!=""){
svmat twfe_bootstrap 
svmat bootstrap_order
sort bootstrap_order1

cap svmat WASeffects_bootstrap	
cap svmat ASeffects_bootstrap	                 
                 
cap gen diff_twfe_ivwas =  twfe_bootstrap1 - IVeffects_bootstrap1
cap gen diff_twfe_ivwas =    twfe_bootstrap1 - WASeffects_bootstrap1

sum diff_twfe_ivwas
scalar diff_twfe_ivwas = r(mean)
scalar N_twfe_ivwas = r(N)
scalar sd_twfe_ivwas   = r(sd)
scalar tsta_twfe_ivwas = scalar(diff_twfe_ivwas)/scalar(sd_twfe_ivwas) //scalar(diff_twfe_ivwas)*sqrt(scalar(N_twfe_ivwas))/scalar(sd_twfe_ivwas)
scalar pval_twfe_ivwas = 2*(1-normal(abs(tsta_twfe_ivwas)))
//save "data_twfe.dta", replace
//Store results
    matrix twfe_ivwas = J(2,6,.)
	matrix twfe_ivwas[2,1] = scalar(diff_twfe_ivwas)
	matrix twfe_ivwas[2,2] = scalar(sd_twfe_ivwas)
	
	sum twfe_bootstrap1
	matrix twfe_ivwas[1,1] = r(mean)
	matrix twfe_ivwas[1,2] = r(sd)
	scalar tsta_twfe = r(mean)/r(sd)
	scalar pval_twfe = 2*(1-normal(abs(tsta_twfe)))
	
	if("percentile" == ""){
		
	matrix twfe_ivwas[1,3] = r(mean)-1.96*r(sd)
	matrix twfe_ivwas[1,4] = r(mean)+1.96*r(sd)
	
	matrix twfe_ivwas[2,3] = scalar(diff_twfe_ivwas) - 1.96*scalar(sd_twfe_ivwas) // */sqrt(scalar(N_twfe_ivwas))
	matrix twfe_ivwas[2,4] = scalar(diff_twfe_ivwas) + 1.96*scalar(sd_twfe_ivwas) // */sqrt(scalar(N_twfe_ivwas))
	}
	else{
		sort diff_twfe_ivwas
		cap drop cum_diff_twfe_ivwas
		gen cum_diff_twfe_ivwas = sum(diff_twfe_ivwas) if diff_twfe_ivwas!=.
		sum cum_diff_twfe_ivwas
		cap drop treshold_XX
		gen treshold_XX = _n if cum_diff_twfe_ivwas == r(min)
		sum treshold_XX
		scalar pval_twfe_ivwas = 2*min(1-r(min)/`bootstrap', r(min)/`bootstrap')
		
		
		_pctile diff_twfe_ivwas, nq(40)
		matrix twfe_ivwas[2,3] = r(r1)
		matrix twfe_ivwas[2,4] = r(r39)
		
		
		sort twfe_bootstrap1
		cap drop cum_twfe_bootstrap1
		gen cum_twfe_bootstrap1 = sum(twfe_bootstrap1) if twfe_bootstrap1!=.
		sum cum_twfe_bootstrap1
		cap drop treshold_XX
		gen treshold_XX = _n if cum_twfe_bootstrap1 == r(min)
		sum treshold_XX
		scalar pval_twfe = 2*min(1-r(min)/`bootstrap', r(min)/`bootstrap')
		
		
		_pctile twfe_bootstrap1, nq(40)
		matrix twfe_ivwas[1,3] = r(r1)
		matrix twfe_ivwas[1,4] = r(r39)
	}
	
	matrix twfe_ivwas[2,5] = scalar(pval_twfe_ivwas)
	matrix twfe_ivwas[2,6] = scalar(tsta_twfe_ivwas)
	
	matrix twfe_ivwas[1,5] = scalar(pval_twfe)
	matrix twfe_ivwas[1,6] = scalar(tsta_twfe)
	
	matrix colnames twfe_ivwas = "Estimate." "SE" "LB CI" "UB CI" "pval." "t"
    if (`iwas_XX' == 1) matrix rownames twfe_ivwas = "TWFE" "TWFE-IVWAS" 
    if (`was_XX' == 1)  matrix rownames twfe_ivwas = "TWFE" "TWFE-WAS" 
	// Modif Felix: Add rownames when AS is specified 
	if (`as_XX' == 1)  matrix rownames twfe_ivwas = "TWFE" "TWFE-AS" 
}

if (`iwas_XX' == 1){
if ("`placebo'"!="0"){
	clear 
	svmat IVplacebos_bootstrap	
	forvalues p = 1/`=max_T'{
	_pctile IVplacebos_bootstrap`p', nq(40)
	scalar LB3_`p'plaXX = r(r1)
	scalar UB3_`p'plaXX = r(r39)
	sum IVplacebos_bootstrap`p'
	scalar sd_delta3_`p'plaXX = r(sd)
}
}
}



use "`OG_dataPathq'.dta", clear

}
}

quietly{

**# Bookmark #4 Output display
macro drop data_1XX //Drop the glob 
cap macro drop data_1plaXX 

scalar nb_rows_XX = 3*(scalar(max_T))
local rownames_XX 
matrix res_mat_XX = J(`=nb_rows_XX',6,.) 

***********************************Fill up the Effects' matrix: START*********************************
*********************************************************************************************

/************************************EFFECTS*****************************************
*************************************************************************************/
//i. Values
forvalues P = 1/3{
forvalues p = 1/`=max_T'{
	scalar P_XX = `=`P''
	scalar index_XX = (scalar(P_XX)-1)*(scalar(max_T))
	if (`as_XX' == 1&`P'==1){
		
			**********************************************************	
	matrix res_mat_XX[`=index_XX' + `p',1] = scalar(delta`P'_`p'XX)
	matrix res_mat_XX[`=index_XX' + `p',2] = sd_delta`P'_`p'XX
	matrix res_mat_XX[`=index_XX' + `p',3] = LB`P'_`p'XX
	matrix res_mat_XX[`=index_XX' + `p',4] = UB`P'_`p'XX
	matrix res_mat_XX[`=index_XX' + `p',5] = N_Switchers`P'_`p'XX
	matrix res_mat_XX[`=index_XX' + `p',6] = Nstayers`P'_`p'XX
	
	}
	if (`was_XX' == 1&`P'==2){
		
			**********************************************************	
	matrix res_mat_XX[`=index_XX' + `p',1] = scalar(delta`P'_`p'XX)
	matrix res_mat_XX[`=index_XX' + `p',2] = sd_delta`P'_`p'XX
	matrix res_mat_XX[`=index_XX' + `p',3] = LB`P'_`p'XX
	matrix res_mat_XX[`=index_XX' + `p',4] = UB`P'_`p'XX
	matrix res_mat_XX[`=index_XX' + `p',5] = N_Switchers`P'_`p'XX
	matrix res_mat_XX[`=index_XX' + `p',6] = Nstayers`P'_`p'XX
	
	}
	if (`iwas_XX' == 1&`P'==3){
		
			**********************************************************	

	matrix res_mat_XX[`=index_XX' + `p',1] = scalar(delta`P'_`p'XX)
	matrix res_mat_XX[`=index_XX' + `p',2] = sd_delta`P'_`p'XX
	matrix res_mat_XX[`=index_XX' + `p',3] = LB`P'_`p'XX
	matrix res_mat_XX[`=index_XX' + `p',4] = UB`P'_`p'XX
	matrix res_mat_XX[`=index_XX' + `p',5] = N_Switchers`P'_`p'XX
	matrix res_mat_XX[`=index_XX' + `p',6] = Nstayers`P'_`p'XX
	
	}
//rownames

if (scalar(P_XX)==1){
	if (`p'>1){
	local rownames_XX `rownames_XX' " as_`p'"
	}
	else{
	local rownames_XX `rownames_XX' "AS"
	}
}

if (scalar(P_XX)==2){
	if (`p'>1){
	local rownames_XX `rownames_XX' "was_`p'"
	}
	else{
	local rownames_XX `rownames_XX' "WAS"
	}
}

if (scalar(P_XX)==3){
	if (`p'>1){
	local rownames_XX `rownames_XX' "iv-was_`p'"
	}
else{
	local rownames_XX `rownames_XX' "IV-WAS"
	}
}
}
}

//ii. Colnames
matrix colnames res_mat_XX= "Estimate" "SE" "LB CI" "UB CI" "Switchers" "Stayers" 

//ii.. Rownames
matrix rownames res_mat_XX = `rownames_XX'
}
***********************************Fill up the matrix: END*********************************
*********************************************************************************************

//local name = ustrupper("`estimator'")

**************************************DISPLAY TABLE: START***********************************
*********************************************************************************************

scalar max_mT = scalar(max_T)-1
//display _newline
/*******************INFOS TO DISPLAY BEFORE THE TABLE*************************/


di as input _skip(34)"{hline 46}"
//Number of observations
if (`as_XX'==1){
	local nb_obs_XX =  scalar(Nstayers1_1XX) + scalar(N_Switchers1_1XX)
}
if (`was_XX'==1){
	local nb_obs_XX =  scalar(Nstayers2_1XX) + scalar(N_Switchers2_1XX)
}
if (`iwas_XX'==1){
	local nb_obs_XX =  scalar(Nstayers3_1XX) + scalar(N_Switchers3_1XX)
}
local nb_obs_XX = round(`nb_obs_XX', 0.001)
local nb_obs_adj_XX = strlen("`nb_obs_XX'")
	di as text _skip(34) "{it: Number of observations}"_skip(4) " =" _skip(`=17-`nb_obs_adj_XX'') "`nb_obs_XX'"
//Estimation method
if (`was_XX'==1){
if ("`estimation_method'" =="" | "`estimation_method'" =="ra"){
	di as text _skip(34) "{it: WAS Estimation method }" "     =  {it:reg. adjustment}"
}

if ("`estimation_method'" =="ps"){
di as text _skip(34) "{it: WAS Estimation method }" "     = {it:propensity-score}"
}

if ("`estimation_method'" =="dr"){
	di as text _skip(34) "{it: WAS Estimation method  }" "    = {it:   doubly-robust}"
}
}

if (`iwas_XX'==1){
if ("`estimation_method'" =="" | "`estimation_method'" =="ra"){
	di as text _skip(34) "{it: IV-WAS Estimation method}" "     =  {it:reg. adjustment}"
}

if ("`estimation_method'" =="ps"){
di as text _skip(34) "{it: IV-WAS Estimation method}" "   = {it:propensity-score}"
}

if ("`estimation_method'" =="dr"){
	di as text _skip(34) "{it: IV-WAS Estimation method}" "   = {it:   doubly-robust}"
}
}

//Polynomial order
if ("`exact_match'"==""){
if ("`cross_validation'"==""){
local pol_adj_XX = strlen("`reg_order'")
di as text _skip(34) "{it: Polynomial order   }"_skip(8)"=" _skip(`=17-`pol_adj_XX'')"`reg_order'"
}
else{
	local pol_adj_XX = strlen("(`reg_order', `logit_bis_order',`logit_Minus_order', `logit_Plus_order')")
di as text _skip(34) "{it: CV Polynomials' orders   }"_skip(2)"=" _skip(`=16-`pol_adj_XX'')"(`reg_order', `logit_bis_order', `logit_Plus_order', `logit_Minus_order')"
}
}

//Type of Common trend assumption
if ("`noextrapolation'" !=""){
	if (scalar(N_drop_noextra_XX)>0){
	di as text _skip(34)  "{it: Common support*}" _skip(12) "="  "{it: no extrapolation}"
	}
	else{
	di as text _skip(34)  "{it: Common support }" _skip(13) "="  "{it: no extrapolation}"
			}
}

if ("`exact_match'"!=""){
	if (scalar(N_drop_noextra_XX)>0){
	di as text _skip(34)  "{it: Common support*}" _skip(12) "=" _skip(2) "{it: exact matching}"
	}
	else{
	di as text _skip(34)  "{it: Common support }" _skip(12) "=" _skip(2) "{it: exact matching}"
		}
}

//Type of switchers
if ("`switchers'" !=""){
	local up_down_XX = strlen("`switchers'")
	di as text _skip(34) "{it: Switchers }" _skip(17) "=" _skip(`=17-`up_down_XX'') "{it:`switchers'}"
}
di as input _skip(34)"{hline 46}"


*******************************************************************************
if ("`cluster'"!=""){
qui levelsof `cluster' 
local nb_cluster_XX = r(r)
local msg_XX = "{it:(Std. err. adjusted for {hi:`nb_cluster_XX'} clusters in {hi:`cluster'})}"
local adjustement_XX = strlen("`msg_XX'")
di as text _skip(`=95-`adjustement_XX'') "`msg_XX'"
}
if (`bootstrap'>0&`iwas_XX'==1){
local msg_XX = "{it:(Bootstrap Std. err. from {hi:`bootstrap'} replications.)}"
local adjustement_XX = strlen("`msg_XX'")
di as text _skip(`=90-`adjustement_XX'') "`msg_XX'"
}

forvalues i = 1/3{
	
	if ("`i'"=="1"&`as_XX' == 1){
		//Effects
		matrix res_mat_1XX`q' = res_mat_XX[ 1..max_T,....]

		di as input "{hline 80}"
		//di as input _skip(35) "Estimation of the `1'(s)"
		di as input _skip(31) "Average Slope (AS)"
		di as input "{hline 80}"
		if ("`disaggregate'" ==""){
			noisily matlist res_mat_1XX`q'[1..1,....]
		}
		else{
			noisily matlist res_mat_1XX`q'
		}
		//Placebos
		if ("`placebo'"!="0"){
			
			// Modif Felix: instead of printing many one line matrices generate one matix combining all those and outputting just one matrix (keep disaggregated as it is)
		
	forvalues placebo_index = 1/`placebo'{
		matrix res_mat_1plaXX`q' = V`placebo_index'res_mat_plaXX[ 1..max_T,....]
		
		if `placebo_index'==1{
			matrix mat_combined1_pl_`q'=res_mat_1plaXX`q'[1..1,....]
		}
		if `placebo_index'>1{
			matrix mat_combined1_pl_`q'=mat_combined1_pl_`q'\res_mat_1plaXX`q'[1..1,....]
		}

		if ("`disaggregate'" !=""){
		di as input "{hline 80}"
		di as input _skip(33) "Placebo(s) AS"
		di as input "{hline 80}"
			noisily matlist res_mat_1plaXX`q'
		}
		
	}
	if ("`disaggregate'" ==""){
			di as input "{hline 80}"
			di as input _skip(33) "Placebo(s) AS"
			di as input "{hline 80}"
			noisily matlist mat_combined1_pl_`q'
		}
	}
	}
	
	if ("`i'"=="2"&`was_XX' == 1){
		
		//Effects
        matrix res_mat_2XX`q' = res_mat_XX[max_T+1..2*max_T,....]

		di as input "{hline 80}"
		//di as input _skip(35) "Estimation of the `2'(s)"
		di as input _skip(27) "Weighted Average Slope (WAS)"
		di as input "{hline 80}"
		
		if ("`disaggregate'" ==""){
			noisily matlist res_mat_2XX`q'[1..1,....]
		}
		else{
			noisily matlist res_mat_2XX`q'
		}
		
		//Placebos
		if ("`placebo'"!="0"){
		forvalues placebo_index = 1/`placebo'{
			matrix res_mat_2plaXX`q' = V`placebo_index'res_mat_plaXX[max_T+1..2*max_T,....]
			
			if `placebo_index'==1{
			matrix mat_combined2_pl_`q'=res_mat_2plaXX`q'[1..1,....]
		}
		if `placebo_index'>1{
			matrix mat_combined2_pl_`q'=mat_combined2_pl_`q'\res_mat_2plaXX`q'[1..1,....]
		}

			if ("`disaggregate'" !=""){
				di as input "{hline 80}"
				di as input _skip(33) "Placebo(s) WAS"
				di as input "{hline 80}"
				noisily matlist res_mat_2plaXX`q'
			}	
		}
			if ("`disaggregate'" ==""){
			di as input "{hline 80}"
			di as input _skip(33) "Placebo(s) WAS"
			di as input "{hline 80}"
			noisily matlist mat_combined2_pl_`q'
		}
		}
	}
	
	if ("`i'"=="3"&`iwas_XX' == 1){
		
		//Effects
        matrix res_mat_3XX`q' = res_mat_XX[2*max_T+1...,....]

		di as input "{hline 80}"
		//di as input _skip(35) "Estimation of the `3'(s)"
		di as input _skip(25) "IV-Weighted Average Slope (IV-WAS)"
		di as input "{hline 80}"
		if ("`disaggregate'" ==""){
				noisily matlist res_mat_3XX`q'[1..1,....]
		}
		else{
				noisily matlist res_mat_3XX`q'
		}	
		
			//Placebos
		if ("`placebo'"!="0"){
		forvalues placebo_index = 1/`placebo'{
	    matrix res_mat_3plaXX`q' = V`placebo_index'res_mat_plaXX[2*max_T+1...,....]
		
		if `placebo_index'==1{
			matrix mat_combined3_pl_`q'=res_mat_3plaXX`q'[1..1,....]
		}
		if `placebo_index'>1{
			matrix mat_combined3_pl_`q'=mat_combined3_pl_`q'\res_mat_3plaXX`q'[1..1,....]
		}
		
		if ("`disaggregate'" !=""){
				di as input "{hline 80}"
				di as input _skip(33) "Placebo(s) IV-WAS"
				di as input "{hline 80}"
				noisily matlist res_mat_3plaXX`q'
			}	
		}
		if ("`disaggregate'" ==""){
			di as input "{hline 80}"
			di as input _skip(33) "Placebo(s) IV-WAS"
			di as input "{hline 80}"
			noisily matlist mat_combined3_pl_`q'
		}
	}
}
}
	di as input " "
	di as input "{hline 80}"
//Number of switchers dropped
if (scalar(N_drop_noextra_XX)==1){
	di as text "* : {it:1 switcher is dropped out of the estimation because its baseline treatment }"
	di as text "{it:does not belong to the support of stayers' baseline treatments.}"
}
if (scalar(N_drop_noextra_XX)>1){
	di as text "* : " scalar(N_drop_noextra_XX) "{it: switchers are dropped out of the estimation because their baseline }"
	di as text "{it: treatments do not belong to the support of stayers' baseline treatments.}"
}

//Number of controls dropped: If exact match
if ("`exact_match'"!=""){
if (scalar(N_drop_c_noextra_XX)==1){
	di as text "* : {it:1 stayer is dropped out of the estimation because its baseline treatment }"
	di as text "{it:does not belong to the support of switchers' baseline treatments.}"
}
if (scalar(N_drop_c_noextra_XX)>1){
	di as text "* : " scalar(N_drop_c_noextra_XX) "{it: stayers are dropped out of the estimation because their baseline }"
	di as text "{it: treatments do not belong to the support of switchers' baseline treatments.}"
}
}

//Quasi-stayers problem
if (`as_XX' == 1 & `was_XX' == 1){
	scalar ratio_sd_XX = scalar(delta1_1XX)/scalar(delta2_1XX)
	if(scalar(ratio_sd_XX)>10&scalar(ratio_sd_XX)!=.){
		di as error "You may have quasi-stayers in your data. The as estimand is likely to be biased."
	}
}

//Test results: WAS vs AS
if ("`as_vs_was'"!=""&`a_vs_w'==2){
	di as input " "

	di as input _skip(26) "Test of difference between AS and WAS"
	di as text "{it:H0: AS = WAS}"
	di as input "{hline 80}"
	noisily matlist as_vs_was_mat
}
di as input "{hline 80}"


//Test results: TWFE  vs IVWAS
if ("`twfe'"!=""){
	di as input " "

	if (`iwas_XX' == 1 ){
	di as input _skip(26) "Test of difference between TWFE and IV-WAS"
		if ("`percentile'"!="") di as text _skip(32) "{it: (Percentile bootstrap method)}"
	else di as text _skip(32) "{it: (Normal bootstrap method)}"
	di as text "{it:H0: TWFE = IV-WAS}"
	}
	if (`was_XX' == 1 ){
	di as input _skip(26) "Test of difference between TWFE and WAS"
	if ("`percentile'"!="") di as text _skip(32) "{it: (Percentile bootstrap method)}"
	else di as text _skip(32) "{it: (Normal bootstrap method)}"
	di as text "{it:H0: TWFE = WAS}"
	}
	// Modif Felix: Add the same heading for the AS 
	if (`as_XX' == 1 ){
	di as input _skip(26) "Test of difference between TWFE and AS"
	if ("`percentile'"!="") di as text _skip(32) "{it: (Percentile bootstrap method)}"
	else di as text _skip(32) "{it: (Normal bootstrap method)}"
	di as text "{it:H0: TWFE = AS}"
	}
	
	di as input "{hline 80}"
	noisily matlist twfe_ivwas
di as input "{hline 80}"
di as text "{it:Values in Column Estimate. are means of bootstrap's point estimates.}"
}


/*******************************************************************************
Drop scalars, this is to avoid using a wrong scalar for a given q
********************************************************************************/
if(`by_quantile'>1){
	local q_index = `q'
	local nobs_quantile`q'  "`nb_obs_XX'"
}
else{
	local q_index 
}
	/*****************************************************************************************
MAKE THE PROGRAM COMPATIBLE WITH ESTOUT: STEP 2 (This code will allows to use estout even if the option by_fd or by_baseline is specifie. The estout will then display a large table with panel for each level of quantile)).
****************************************************************************************/
matrix b_q = J(1,`total_estimator',0)
matrix V_q = J(`total_estimator', `total_estimator', 0)
local if_pla = 1
if ("`placebo'"!="0"){
	matrix V_q = J(2*(`total_estimator'), 2*(`total_estimator'), 0)
	matrix b_q = J(1,2*`total_estimator',0)
	local if_pla = 2
}
 //local colnames : colnames b // local colnames : subinstr local colnames "c2" "Placebo1", all
if(`as_XX' == 1&`was_XX' == 0) {
	if (scalar(delta1_1XX)!=.) matrix b_q[1,1] == delta1_1XX
	local colnames "`colnames' AS`q_index'"
	if (scalar(sd_delta1_1XX)!=.) matrix V_q[1,1] = sd_delta1_1XX^2
}

if(`was_XX' == 1&`as_XX' == 0) {
	if (scalar(delta2_1XX)!=.) matrix b_q[1,1] = delta2_1XX
	local colnames "`colnames' WAS`q_index'"
	if (scalar(sd_delta2_1XX)!=.) matrix V_q[1,1] = sd_delta2_1XX^2
}

if(`=`as_XX' +`was_XX'' == 2) {
	
	if (scalar(delta1_1XX)!=.) matrix b_q[1,1] == delta1_1XX
	local colnames "`colnames' AS`q_index'"
	if (scalar(sd_delta1_1XX)!=.) matrix V_q[1,1] = sd_delta1_1XX^2
	
	if (scalar(delta2_1XX)!=.) matrix b_q[1,2] = delta2_1XX
	local colnames "`colnames' WAS`q_index'"
	if (scalar(delta2_1XX)!=.) matrix V_q[2,2] = sd_delta2_1XX^2
}

if (`iwas_XX'==1){
	if (scalar(delta3_1XX)!=.) matrix b_q[1,1] = delta3_1XX	
	local colnames "`colnames' IV-WAS`q_index'"
	if (scalar(delta3_1XX)!=.) matrix V_q[1,1] = sd_delta3_1XX^2
}

//b of the placebos
if ("`placebo'"!="0"){
	
	if(`as_XX' == 1&`was_XX' == 0) {
	if (scalar(delta1_1plaXX)!=.) matrix b_q[1,2] == delta1_1plaXX
	local colnames "`colnames' PlaceboAS`q_index'"
	if (scalar(sd_delta1_1plaXX)!=.) matrix V_q[2,2] = sd_delta1_1plaXX^2
}

if(`was_XX' == 1&`as_XX' == 0) {
	if (scalar(delta2_1plaXX)!=.) matrix b_q[1,2] = delta2_1plaXX
	local colnames "`colnames' PlaceboWAS`q_index'"
	if (scalar(sd_delta2_1plaXX)!=.) matrix V_q[2,2] = sd_delta2_1plaXX^2
}

if(`=`as_XX' +`was_XX'' == 2) {
	
	if (scalar(delta1_1plaXX)!=.) matrix b_q[1,3] == delta1_1plaXX
	local colnames "`colnames' PlaceboAS`q_index'"
	if (scalar(sd_delta1_1plaXX)!=.) matrix V_q[3,3] = sd_delta1_1plaXX^2
	
	if (scalar(delta2_1plaXX)!=.) matrix b_q[1,4] = delta2_1plaXX
	local colnames "`colnames' PlaceboWAS`q_index'"
	if (scalar(delta2_1plaXX)!=.) matrix V_q[4,4] = sd_delta2_1plaXX^2
}

if (`iwas_XX'==1){
	if (scalar(delta3_1plaXX)!=.) matrix b_q[1,2] = delta3_1plaXX	
	local colnames "`colnames' Placebo`q_index'"
	if (scalar(delta3_1plaXX)!=.) matrix V_q[2,2] = sd_delta3_1plaXX^2
}

}

//cap scalars_to_drop //This program is defined below
	mata : b_q = st_matrix("b_q")
	mata : V_q = st_matrix("V_q")
	
	//Fill the main matrix for estout
	mata : b[, `=`if_pla'*`total_estimator'*(`q'-1)+1'::`=`if_pla'*`total_estimator'*`q''] = b_q[, 1::`=`if_pla'*`total_estimator''] 

	mata : V[`=`if_pla'*`total_estimator'*(`q'-1)+1'::`=`if_pla'*`total_estimator'*`q'', `=`if_pla'*`total_estimator'*(`q'-1)+1'::`=`if_pla'*`total_estimator'*`q''] = V_q[1::`=`if_pla'*`total_estimator'', 1::`=`if_pla'*`total_estimator''] 
		
	mata : st_matrix("b_q", b_q)
	mata : st_matrix("V_q", V_q)
	
	mata : st_matrix("b", b)
	mata : st_matrix("V", V)
	
/*matlist b_q
matlist V_q
matlist b
matlist V
*/
use "`OG_dataPathq'.dta", clear

}
//>MAIN: Restore the inputted dataset

	restore

matrix colnames b = `colnames'
matrix colnames V = `colnames'
matrix rownames V = `colnames'
cap ereturn post b V, obs(`nb_obs_XX') depname("`depname'")
if _rc!=0{
	di ""
	di as error "Some of the effects/placebos or their standard errors could not be computed so e(b)/e(V) will not be defined."
}
//ereturn matrix testt = J(5, 2, 0)
if (`by_quantile'>1){
forvalues q = 1/`by_quantile'{
	return_label, by_quantile(`by_quantile') by_fd(`by_fd') q(`q') as(`as_XX') was(`was_XX') iwas(`iwas_XX')
	ereturn local label_quantile`q'  "`s(label_quantile`q')'"
	ereturn local nobs_quantile`q'  "`nobs_quantile`q''"
}
}
**************************************DISPLAY TABLE: END*************************************
*********************************************************************************************
local by_index_XX = ""
		/****************************************************************************
		I. If program by'd
		****************************************************************************/
//ERETURNS
if (_by()){
	local by_index_XX = _byindex()	
	//if in addition by_quantile is specified
	forvalues q=1/`by_quantile'{
		//Store each by level results in a matrix tagged with the level
	if (`as_XX'==1)   matrix  AS_`by_index_XX'XX`q'    = res_mat_1XX`q'
	if (`was_XX'==1)  matrix  WAS_`by_index_XX'XX`q'   = res_mat_2XX`q'
	if (`iwas_XX'==1) matrix  IWAS_`by_index_XX'XX`q'  = res_mat_3XX`q'
	
	if ("`placebo'"!="0"){
		if (`as_XX'==1)   matrix  PlaceboAS_`by_index_XX'XX`q'   = res_mat_1plaXX`q'
		if (`was_XX'==1)  matrix  PlaceboWAS_`by_index_XX'XX`q'  = res_mat_2plaXX`q'
		if (`iwas_XX'==1) matrix  PlaceboIWAS_`by_index_XX'XX`q' = res_mat_3plaXX`q'
	}
	}
	
	if (_bylastcall()){ 
		/****************************************************************************
		I.1. Ereturn matrices
		****************************************************************************/
		//ereturn clear
		matrix All_XX =  J(`nb_level_by_XX',6,.)
					
		forvalues level=1/`nb_level_by_XX'{
		
		if (`by_quantile'==1){
		if (`as_XX'==1)  ereturn matrix AS_`level'    = AS_`level'XX1
		if (`was_XX'==1)  ereturn matrix WAS_`level'   = WAS_`level'XX1 
		if (`iwas_XX'==1) ereturn matrix IWAS_`level'  = IWAS_`level'XX1

		if ("`placebo'"!="0"){
			if (`as_XX'==1)   ereturn matrix  PlaceboAS_`level'   =   PlaceboAS_`level'XX1
			if (`was_XX'==1)  ereturn matrix  PlaceboWAS_`level'  =  PlaceboWAS_`level'XX1
			if (`iwas_XX'==1) ereturn matrix  PlaceboIWAS_`level' = PlaceboIWAS_`level'XX1
		}
		}
		else{
				forvalues q=1/`by_quantile'{
						if (`as_XX'==1)  ereturn matrix AS_`level'_`q'    = AS_`level'XX`q'
						if (`was_XX'==1)  ereturn matrix WAS_`level'_`q'   = WAS_`level'XX`q' 
						if (`iwas_XX'==1) ereturn matrix IWAS_`level'_`q'  = IWAS_`level'XX`q' 

						if ("`placebo'"!="0"){
							if (`as_XX'==1)   ereturn matrix  PlaceboAS_`level'_`q'   =   PlaceboAS_`level'XX`q'
							if (`was_XX'==1)  ereturn matrix  PlaceboWAS_`level'_`q'  =  PlaceboWAS_`level'XX`q'
							if (`iwas_XX'==1) ereturn matrix  PlaceboIWAS_`level'_`q' = PlaceboIWAS_`level'XX`q'
						}
				}
		}
		}
	} //End if _bylastcall()
	
	

}
		/****************************************************************************
		II. If program NOT by'd
		****************************************************************************/
else{ 
	
	//II.1 If by_quantile is specified
	
if (`by_quantile'>1){
	//ereturn clear
			forvalues q=1/`by_quantile'{
						if (`as_XX'==1)  ereturn matrix AS_`q'    = res_mat_1XX`q'
						if (`was_XX'==1)  ereturn matrix WAS_`q'   = res_mat_2XX`q'
						if (`iwas_XX'==1) ereturn matrix IWAS_`q'  = res_mat_3XX`q'

						if ("`placebo'"!="0"){
							if (`as_XX'==1)   ereturn matrix  PlaceboAS_`q'   =    res_mat_1plaXX`q'
							if (`was_XX'==1)  ereturn matrix  PlaceboWAS_`q'  =   res_mat_2plaXX`q'
							if (`iwas_XX'==1) ereturn matrix  PlaceboIWAS_`q' =  res_mat_3plaXX`q'
						}
				}	
}
else{
	//ereturn clear
if (`as_XX' == 1) ereturn matrix AS    = res_mat_1XX1
if (`was_XX' == 1) ereturn matrix WAS  = res_mat_2XX1
if (`iwas_XX' == 1) ereturn matrix WAS = res_mat_3XX1

		if ("`placebo'"!="0"){
			if (`as_XX' == 1) ereturn matrix PlaceboAS    = res_mat_1plaXX1
			if (`was_XX' == 1) ereturn matrix PlaceboWAS  = res_mat_2plaXX1
			if (`iwas_XX' == 1) ereturn matrix PlaceboWAS = res_mat_3plaXX1			
		}
}

}

/*****************************************************************************************
The differents graphs requested
****************************************************************************************/
quietly{
	if ("`bys_graph_off'"==""&(_by()==1|`by_quantile'>1)){
		local graph_input =""

	local byed = _by()
	local quant = `by_quantile'>1
	
		/****************************************************************************
	    When only bysort or only by_quantile is specified
		****************************************************************************/

	if (`byed'+`quant'==1){ 
		
		if (`by_quantile'>1){
			local nb_level_XX = `by_quantile'
			local end_by = 1
		}
		
		if (_by()){
			local nb_level_XX = `nb_level_by_XX'
			local end_by = _bylastcall()
		}
		if (`end_by'==1){
		local e_p_XX = ""
		if ("`placebo'"!="0"){
			local e_p_XX "" "Placebo"
		}
        foreach e_p in "`e_p_XX'"{
		preserve 
		//// Storing the information we need
		matrix `e_p'AS_XX = J(`nb_level_XX', 4,.)
		matrix `e_p'WAS_XX = J(`nb_level_XX', 4,.)
		matrix `e_p'IWAS_XX = J(`nb_level_XX', 4,.)

		forvalues level=1/`nb_level_XX'{
			forvalues i=1/4{
         if (`as_XX'==1) matrix `e_p'AS_XX[`level',`i'] = e(`e_p'AS_`level')[1,`i']
         if (`was_XX'==1) matrix `e_p'WAS_XX[`level',`i'] = e(`e_p'WAS_`level')[1,`i']
         if (`iwas_XX'==1) matrix `e_p'IWAS_XX[`level',`i'] = e(`e_p'IWAS_`level')[1,`i']
			}	
		}
		//Keep only the by vars for a matter of labelling the graph
		if (_by()){
		sort `_byvars'
		cap drop by_vars_XX
		egen by_vars_XX = group(`_byvars'), label(by_vars_XX, replace)
		keep by_vars_XX
		bysort by_vars_XX: gen unique_label = _n==1
		keep if unique_label
		label var by_vars_XX "`_byvars'"
		}
		if (`by_quantile'>1){
			clear
			set obs `by_quantile'
			cap drop by_vars_XX 
			gen by_vars_XX = _n
			forvalues q=1/`by_quantile'{
				
				//I will use this to label the graph
					local lb = scalar(r`=`q'-1'_rr)
					local ub = scalar(r`q'_rr)
					local label_q_`q' = "[`lb'; `ub'["
					label define by_quantile_label  `q' "`label_q_`q''", add
				//label define by_quantile_label `q' "Block `q'", add
			}
			label values by_vars_XX by_quantile_label
			if (`by_fd'>1){
				if (`as_XX'==1| `was_XX'==1) label var by_vars_XX "|{&Delta}D{sub:t}|"
				if (`iwas_XX'==1) label var by_vars_XX "|{&Delta}Z{sub:t}|"
			}
			else{
				if (`as_XX'==1| `was_XX'==1) label var by_vars_XX "D{sub:t-1}"
				if (`iwas_XX'==1) label var by_vars_XX "Z{sub:t-1}"				
			}
		}
		
		if (`as_XX'==1) {
			svmat `e_p'AS_XX
			forvalues i=1/4{
				rename `e_p'AS_XX`i' All_XX`i'_1
			}
		}
		if (`was_XX'==1){
			svmat `e_p'WAS_XX
			forvalues i=1/4{
				rename `e_p'WAS_XX`i' All_XX`i'_2
			}
		}
		if (`iwas_XX'==1) {
			svmat `e_p'IWAS_XX
			forvalues i=1/4{
				rename `e_p'IWAS_XX`i' All_XX`i'_3
			}			
		}
		
		reshape long All_XX1_ All_XX2_ All_XX3_ All_XX4_, i(by_vars_XX)
		rename _j estimator
		label define estimator 1"AS" 2 "WAS" 3 "IV-WAS"
		label values estimator estimator
		
		//// Setting up the graph
		
		// Define fixed graph options as they are implemented as flexible locals
		local graph_input "(dot All_XX1_ by_vars_XX, lpattern(solid)) (rcap All_XX3_ All_XX4_ by_vars_XX)"
		
        if ("`e_p'"==""){
			local e_p = "Effect"
		}
		cap graph drop `e_p'_graph
		twoway `graph_input', xlabel(1[1]`nb_level_XX', valuelabel angle(45) labsize(small)) graphregion(color(white)) plotregion(color(white)) by(estimator, legend(off) rows(1) title("`e_p's") note("", size(tiny)))  `graph_options' name(`e_p'_graph, replace) nodraw
		
		restore
	}
	if ("`placebo'"!="0"){
	graph combine Effect_graph Placebo_graph, cols(1) name(Graph1, replace )
	}
	else{
	graph combine Effect_graph, cols(1) name(Graph1, replace)
	
	}
	}
	}
		
else{
	if (_bylastcall()){
		/****************************************************************************
	    When both bysort and by_quantile are specified
		****************************************************************************/
		
		local e_p_XX = ""
		if ("`placebo'"!="0"){
			local e_p_XX "" "Placebo"
		}
        foreach e_p in "`e_p_XX'"{
			// Initialize color pattern for multiple graphs wihen by is specified 
local col1 "midblue"
local col2 "red"  
local col3 "green"    
local col4 "magenta"   
local col5 "gold"      
local col6 "lime"  
		preserve 
				//Keep only the by vars for a matter of labelling the graph
				sort `_byvars'
				cap drop byed_vars_XX
				egen byed_vars_XX = group(`_byvars'), label(byed_vars_XX, replace)
				keep byed_vars_XX
				bysort byed_vars_XX: gen unique_label = _n==1
				keep if unique_label
				label var byed_vars_XX "`_byvars'"
				
				// The by quantile variable
			//clear
			if (`by_quantile'>`nb_level_by_XX'){
			set obs `by_quantile'
			}
			cap drop by_vars_1XX 
			gen by_vars_1XX = _n - 1/10 if _n<=`by_quantile'
			cap drop by_vars_XX 
			gen by_vars_XX = _n if _n<=`by_quantile'
			cap drop y 
			gen y =.
			local graph_input ="(dot y by_vars_XX, lpattern(solid))"
			forvalues q=1/`by_quantile'{
				
				//I will use this to label the graph
					local lb = scalar(r`=`q'-1'_rr)
					local ub = scalar(r`q'_rr)
					local label_q_`q' = "[`lb'; `ub'["
					label define by_quantile_label  `q' "`label_q_`q''", add
				//label define by_quantile_label `q' "Block `q'", add
			}
			label values by_vars_XX by_quantile_label

			
		//// Storing the information we need
		local nb_level_XX = `by_quantile'
		forvalues by_level = 1/`nb_level_by_XX'{
		matrix `e_p'AS_`by_level'XX = J(`nb_level_XX', 4,.)
		matrix `e_p'WAS_`by_level'XX = J(`nb_level_XX', 4,.)
		matrix `e_p'IWAS_`by_level'XX = J(`nb_level_XX', 4,.)
		}
		local var_names ""
		forvalues by_level = 1/`nb_level_by_XX'{
			if (`by_level'>1){
			cap drop by_vars_`by_level'XX 
			gen by_vars_`by_level'XX = (_n - 0.1) + 0.2/(`=`nb_level_by_XX'-1')*(`=`by_level'-1')
			label var by_vars_`by_level'XX "|deltaD|"
			}
		local var_names  "`var_names'" "All_`by_level'XX1_"
		local var_names "`var_names'" "All_`by_level'XX2_"
		local var_names "`var_names'" "All_`by_level'XX3_"
		local var_names "`var_names'" "All_`by_level'XX4_"
		
		forvalues level=1/`nb_level_XX'{
			forvalues i=1/4{
         if (`as_XX'==1) matrix `e_p'AS_`by_level'XX[`level',`i'] = e(`e_p'AS_`by_level'_`level')[1,`i']
         if (`was_XX'==1) matrix `e_p'WAS_`by_level'XX[`level',`i'] = e(`e_p'WAS_`by_level'_`level')[1,`i']
         if (`iwas_XX'==1) matrix `e_p'IWAS_`by_level'XX[`level',`i'] = e(`e_p'IWAS_`by_level'_`level')[1,`i']
			}
		}	
		if (`as_XX'==1) {
			svmat `e_p'AS_`by_level'XX
			forvalues i=1/4{
				rename  `e_p'AS_`by_level'XX`i' All_`by_level'XX`i'_1
			}
		}
		if (`was_XX'==1){
			svmat `e_p'WAS_`by_level'XX
			forvalues i=1/4{
				rename `e_p'WAS_`by_level'XX`i' All_`by_level'XX`i'_2
			}
		}
		if (`iwas_XX'==1) {
			svmat `e_p'IWAS_`by_level'XX
			forvalues i=1/4{
				rename `e_p'IWAS_`by_level'XX`i' All_`by_level'XX`i'_3
			}			
		}
			
					
		// Define fixed graph options as they are implemented as flexible locals
		
		
		local graph_input = "`graph_input'" + "(dot All_`by_level'XX1_ by_vars_`by_level'XX, lpattern(solid) mcolor( `col`by_level'')) (rcap All_`by_level'XX3_ All_`by_level'XX4_ by_vars_`by_level'XX, lcolor(`col`by_level''))"
		}
		
		drop if by_vars_XX==. //To prevent reshape from craching.
		
		reshape long `var_names' , i(by_vars_XX)
		rename _j estimator
		label define estimator 1"AS" 2 "WAS" 3 "IV-WAS"
		label values estimator estimator

		//// Setting up the graph

		// Label of the legend
		forvalues by_level = 1/`nb_level_by_XX'{
		local val_lab "`: label byed_vars_XX `by_level''"
		label var All_`by_level'XX1_  "`val_lab'"
		}
        if ("`e_p'"==""){
			local e_p = "Effect"
		//Orders of the legend
		forvalues by_level = 1/`nb_level_by_XX'{
		local leg_order = "`leg_order' `=2*`by_level''" 
		}
		}
	
		cap graph drop `e_p'_graph
		if (`by_fd'>1){
			if (`as_XX'==1| `was_XX'==1) local xtitle =  "{&Delta}D{sub:t}" 
			if (`iwas_XX'==1) local xtitle =  "{&Delta}Z{sub:t}" 
		}
		else{
			if (`as_XX'==1| `was_XX'==1) local xtitle =  "D{sub:t-1}" 
			if (`iwas_XX'==1) local xtitle =  "Z{sub:t-1}" 			
		}
		twoway `graph_input', xlabel(1[1]`=`nb_level_XX'', valuelabel angle(45) labsize(small)  nogrid tstyle(minor_notick) ) by(estimator,  rows(1) title("`e_p's") note("", size(tiny) )  graphregion(color(white)) plotregion(color(white)))  `graph_options' name(`e_p'_graph, replace) nodraw legend(order(`leg_order') subtitle("`_byvars'")) xtitle("`xtitle'")
		restore
	}
	if ("`placebo'"!="0"){
	graph combine Effect_graph Placebo_graph, cols(1) name(Graph1, replace )
	}
	else{
	graph combine Effect_graph, cols(1) name(Graph1, replace)
	}
}
}
}
}


if ("`graph_off'"==""){
	preserve 
	if (`as_XX' == 1){
		
	quietly{ // Modif Felix: supress the gen/replace output
		
			clear 
	matrix graph_inputAS = J(`=`placebo'+2', 6, 0)
	mata: graph_inputAS = st_matrix("graph_inputAS")
	mata: res_mat_XX = st_matrix("res_mat_XX")
	mata: max_T = st_numscalar("max_T")
	mata: graph_inputAS[1, ] = res_mat_XX[1, ]

	forvalues placebo_index = 3/`=`placebo'+2'{
		matrix tmp = V`=`placebo_index'-2'res_mat_plaXX
		mata: res_mat_tmpXX = st_matrix("tmp")
		scalar p_index = `placebo_index'
		mata: p_index = st_numscalar("p_index")
		mata: graph_inputAS[p_index,.] = res_mat_tmpXX[1,.]
		cap matrix drop tmp
		cap scalar drop p_index
	}
	
	mata: st_matrix("graph_inputAS", graph_inputAS)
	svmat graph_inputAS
	
	// Define fixed graph options as they are implemented as flexible locals
	cap drop time_to_treat
	gen time_to_treat = _n if !missing(graph_inputAS1)
	replace time_to_treat = 0 if time_to_treat ==2
	replace time_to_treat = 2 -time_to_treat if !inlist(time_to_treat, 1, 0)
	
local graph_input "(connected graph_inputAS1 time_to_treat, lpattern(solid)) (rcap graph_inputAS4 graph_inputAS3 time_to_treat)"
local graph_options "legend(off)"

	} // end quietly	

if (`was_XX' == 1& `as_XX' == 1) twoway `graph_input', xlabel(`=-`placebo''[1]1) xtitle("Relative time", size(large)) title("AS", size(large)) graphregion(color(white)) plotregion(color(white)) `graph_options' name(as, replace) nodraw
else twoway `graph_input', xlabel(`=-`placebo''[1]1) xtitle("Relative time", size(large)) title("AS", size(large)) graphregion(color(white)) plotregion(color(white)) `graph_options' name(as, replace) 
}

	if (`was_XX' == 1){
	
	quietly{ // Modif Felix: supress the gen/replace output
		
	matrix graph_inputWAS = J(`=`placebo'+2', 6, 0)
	mata: graph_inputWAS = st_matrix("graph_inputWAS")
	mata: res_mat_XX = st_matrix("res_mat_XX")
	mata: max_T = st_numscalar("max_T")
	mata: graph_inputWAS[1,.] = res_mat_XX[max_T+1,.]


	forvalues placebo_index = 3/`=`placebo'+2'{
		matrix tmp = V`=`placebo_index'-2'res_mat_plaXX
		scalar p_index = `placebo_index'
		mata: res_mat_tmpXX = st_matrix("tmp")
		mata: p_index = st_numscalar("p_index")
		mata: graph_inputWAS[p_index,.] = res_mat_tmpXX[max_T+1,.]
		cap matrix drop tmp
		cap scalar drop p_index
	}
	
	mata: st_matrix("graph_inputWAS", graph_inputWAS)
	svmat graph_inputWAS
	
	// Define fixed graph options as they are implemented as flexible locals
	cap drop time_to_treat
	gen time_to_treat = _n if !missing(graph_inputWAS1)
	replace time_to_treat = 0 if time_to_treat ==2
	replace time_to_treat = 2 -time_to_treat if !inlist(time_to_treat, 1, 0)
	
local graph_input "(connected graph_inputWAS1 time_to_treat, lpattern(solid)) (rcap graph_inputWAS4 graph_inputWAS3 time_to_treat)"
local graph_options "legend(off)"

	}

if (`was_XX' == 1& `as_XX' == 1) twoway `graph_input', xlabel(`=-`placebo''[1]1) xtitle("Relative time", size(large)) title("WAS", size(large)) graphregion(color(white)) plotregion(color(white)) `graph_options' name(was, replace) nodraw
else twoway `graph_input', xlabel(`=-`placebo''[1]1) xtitle("Relative time", size(large)) title("WAS", size(large)) graphregion(color(white)) plotregion(color(white)) `graph_options' name(was, replace) 
}

	// Modif Felix: Add graph for IV-WAS
	if (`iwas_XX' == 1){
	
	quietly{ // Modif Felix: supress the gen/replace output
		
	matrix graph_inputIWAS = J(`=`placebo'+2', 6, 0)
	mata: graph_inputIWAS = st_matrix("graph_inputIWAS")
	mata: res_mat_XX = st_matrix("res_mat_XX")
	mata: max_T = st_numscalar("max_T")
	mata: graph_inputIWAS[1,.] = res_mat_XX[2*max_T+1,.]


	forvalues placebo_index = 3/`=`placebo'+2'{
		matrix tmp = V`=`placebo_index'-2'res_mat_plaXX
		scalar p_index = `placebo_index'
		mata: res_mat_tmpXX = st_matrix("tmp")
		mata: p_index = st_numscalar("p_index")
		mata: graph_inputIWAS[p_index,.] = res_mat_tmpXX[2*max_T+1,.]
		cap matrix drop tmp
		cap scalar drop p_index
	}
	
	mata: st_matrix("graph_inputIWAS", graph_inputIWAS)
	svmat graph_inputIWAS
	
	// Define fixed graph options as they are implemented as flexible locals
	cap drop time_to_treat
	gen time_to_treat = _n if !missing(graph_inputIWAS1)
	replace time_to_treat = 0 if time_to_treat ==2
	replace time_to_treat = 2 -time_to_treat if !inlist(time_to_treat, 1, 0)
	
local graph_input "(connected graph_inputIWAS1 time_to_treat, lpattern(solid)) (rcap graph_inputIWAS4 graph_inputIWAS3 time_to_treat)"
local graph_options "legend(off)"

	}

twoway `graph_input', xlabel(`=-`placebo''[1]1) xtitle("Relative time", size(large)) title("IV-WAS", size(large)) graphregion(color(white)) plotregion(color(white)) `graph_options' name(iwas, replace)
}

	restore 

	//di as error "Im here"
	//matlist V2res_mat_plaXX
	//matlist graph_inputAS
	
	if (`was_XX' == 1& `as_XX' == 1){
	graph combine as was, cols(1) name(all, replace ) /*title(DID from last period before treatment changes (t=0) to t, size(medium))*/
	}
} // Felix: end graph_off	
	
	
end

*******************************************************************************************
//Program 2 : This program compute the estimators for each two successive time periods
*******************************************************************************************

capture program drop did_multiplegt_stat_pairwise
program did_multiplegt_stat_pairwise, eclass
	version 12.0
	syntax varlist(min=4 max=5 numeric) [if] [in] [, estimator(string) ORder(integer 1) NOEXTRApolation weights(varlist numeric) switchers(string) pairwise(integer 2) data_1XX(string) as(integer 0) was(integer 0) iwas(integer 0) estimation_method(string) placebo(integer 0) exact_match cluster(varlist max=1) quantile(integer 1) by_fd(integer 1) by_baseline(integer 1) other_treatments(varlist numeric) controls(varlist numeric)  bootstrap(integer 0) reg_order(integer 1) logit_bis_order(integer 1)  logit_Plus_order(integer 1) logit_Minus_order(integer 1) cross_validation(string)]
	
quietly{
//> CORE preserve
tempfile OG_dataPathcore
	save "`OG_dataPathcore'.dta", replace
	
//IV method:
local IV_feed_XX = "no"

if ("`5'"!=""&"`5'"!=","){
local IV_feed_XX = "yes"
}

if ("`IV_feed_XX'"=="no"&(`iwas' == 1|"`estimator'" == "")){
	//di as error "To compute the iwas you must specify the IV variable."
	local iwas = 0
	//exit
}

**# Bookmark #1: Format the data: Subselect the two times we are interested in
if ("`placebo'"=="0"){
keep if inlist(T_XX,  `pairwise'-1, `pairwise')


}
else{
	
	//keep if inlist(T_XX,  `pairwise'-2, `pairwise'-1, `pairwise')
//Controls: Take X_{t-1} (automatically done since we drop if t==2) if Effect and X_{t-2} if Placebo1 and X_{t-3} if Placebo2
if ("`controls'"!=""){
	foreach control in `controls'{
		cap drop temp_`control'XX temp_`control'2XX
xtset ID_XX T_XX
		bysort ID_XX: gen temp_`control'2XX  = L.`control'
		bysort ID_XX: egen temp_`control'XX = mean(temp_`control'2XX )
		replace `control' = temp_`control'XX
	}
}
//keep if inlist(T_XX,  `pairwise'-2, `pairwise'-1, `pairwise')
keep if inlist(T_XX,  `pairwise'-`placebo' -1, `pairwise'-`placebo', `pairwise'-1, `pairwise')
local pla = "pla"
//di as error "inlist(T_XX,  `pairwise'-`placebo' -1, `pairwise'-`placebo', `pairwise'-1, `pairwise')"
}
 
//di as error "inlist(T_XX,  `pairwise'-1, `pairwise')"

//Check if one of the two periods was a gap:
bysort T_XX: gegen tsfilled_minXX = min(tsfilled_XX)
sum tsfilled_minXX
scalar gap_`pairwise'`pla'XX =  r(max) 


*/

sort T_XX
gegen Tbis_XX = group(T_XX)
replace T_XX = Tbis_XX
drop Tbis_XX

xtset ID_XX  T_XX

//Generate deltaY = Y_t - Y_(t-1)
bysort ID_XX: gen deltaY_XX = D.Y_XX
///>put it  at the same level of Y_(t-1)
if ("`placebo'"=="0"){
	bysort ID_XX: gegen delta_temp = mean(deltaY_XX)
	replace deltaY_XX = delta_temp
	drop delta_temp
}
else{
	gen delta_temp = deltaY_XX if T_XX == 2 // = Y_{t-1} - Y_{t-2} or in general Y_{t-placebo} - Y_{t-placebo-1}
	bysort ID_XX: gegen delta_temp2 = mean(delta_temp)
	replace deltaY_XX = delta_temp2
	drop delta_temp2 
	drop delta_temp
}


//Generate deltaD_t = D_t - D_(t-1)
sort ID_XX T_XX
bysort ID_XX : gen deltaD_XX = D.D_XX

if ("`placebo'"!="0"&(`as'==1|`was'==1)){
	cap drop inSamplePlacebo_tempXX
	cap drop inSamplePlacebo_XX
	gen inSamplePlacebo_tempXX = (deltaD_XX==0)&(T_XX==2) if (deltaD_XX!=.) //Units such that D_{t-2} = D_{t-1} or in general such that D_{t-placebo} - D_{t-placebo-1}
	bysort ID_XX: gegen inSamplePlacebo_XX = max(inSamplePlacebo_tempXX)
	
	//Only keep Units such that D_{t-2} = D_{t-1}
	//keep if inSamplePlacebo_XX==1
	drop if T_XX == 1 //We do not need that line since we've already computed Y_{t-2} - Y_{t-1}, and selected Units such that D_{t-2} = D_{t-1} // And evrything that follows is the same as the computation of the effects:)
	if(`placebo'>1) drop if T_XX == 2
	if(`placebo'==1) bysort ID_XX: replace deltaD_XX = . if T_XX!=3 //We need the DeltaD_t only and we will take the mean after to keep the same value for all the dates
	if(`placebo'>1) bysort ID_XX: replace deltaD_XX = . if T_XX!=4
	
}

if (`iwas' == 1){
	//IV
	cap drop deltaZ_XX
	cap drop SI_XX
	cap drop Z_XX
	cap drop outOfBoundsiV_XX
	
	gen Z_XX = `5'
	
	//Generate deltaZ_t = Z_t - Z_(t-1)
	sort ID_XX T_XX
	bysort ID_XX : gen deltaZ_XX = D.Z_XX
	if ("`placebo'"!="0"){
		
		/*Start Correction*/
		//First we need deltaD_{t-1} instead of deltaD_t: No, in fact we need D_t to avoid dividing by 0 when the FS PT is verified; Ask Clement, why not showing the Placebos of the FS and the RF when IV is specified.
	replace deltaD_XX = . if T_XX!= 3 // = D_{t-1} - D_{t-2}

	
		///*End Correction*/
		
	cap drop inSamplePlaceboIV_tempXX
	cap drop inSamplePlaceboIV_XX
	gen inSamplePlaceboIV_tempXX = (deltaZ_XX==0)&(T_XX==2) //Units such that Z_{t-2} = Z_{t-1}
	bysort ID_XX: gegen inSamplePlacebo_XX = max(inSamplePlaceboIV_tempXX)
	
	//Only keep Units such that Z_{t-2} = Z_{t-1}
	//keep if inSamplePlacebo_XX==1
	drop if T_XX == 1 //We do not need that line since we've already computed Y_{t-2} - Y_{t-1}, and selected Units such that Z_{t-2} = Z_{t-1} // And evrything that follows is the same as the computation of the effects:)
	if(`placebo'>1) drop if T_XX == 2
	if(`placebo'==1) bysort ID_XX: replace deltaZ_XX = . if T_XX!=3 //We need the DeltaZ_t only and we will take the mean after to keep the same value for all the dates
	if(`placebo'>1) bysort ID_XX: replace deltaZ_XX = . if T_XX!=4
	}
	
}
		
if (_N == 0){ //If the placebo cannot be estimated (because the subsample {i: D_{t-2} = D_{t-1}} is empty), create the variables,and scalars and set them to .
	if (`was'==1|`as'==1){
	forvalues i=1/2{
	scalar delta`i'_`pairwise'`pla'XX  = . // 0
	scalar sd_delta`i'_`pairwise'`pla'XX = .
	scalar LB`i'_`pairwise'`pla'XX = .
	scalar UB`i'_`pairwise'`pla'XX = .
	
	cap drop Phi`i'_`pairwise'`pla'XX
	gen Phi`i'_`pairwise'`pla'XX = .
	
	scalar N_Switchers`i'_`pairwise'`pla'XX = .
	scalar Nstayers`i'_`pairwise'`pla'XX = .
		
	}
	cap drop absdeltaD_`pairwise'`pla'XX
	cap drop S_`pairwise'`pla'XX
	gen absdeltaD_`pairwise'`pla'XX = .
	gen S_`pairwise'`pla'XX = .
	scalar EabsdeltaD_`pairwise'`pla'XX = 0
	scalar P_`pairwise'`pla'XX = 0
	cap drop used_in_`pairwise'_XX
	gen used_in_`pairwise'_XX=.
	
	
	}
else{
	scalar delta3_`pairwise'`pla'XX  = . //0
	scalar sd_delta3_`pairwise'`pla'XX = .
	scalar LB3_`pairwise'`pla'XX = .
	scalar UB3_`pairwise'`pla'XX = .
	scalar denom_deltaIV`pairwise'`pla'XX = 0
	cap drop Phi3_`pairwise'`pla'XX
	gen Phi3_`pairwise'`pla'XX = .
	gen innerSumIV_denom_`pairwise'`pla'XX = .
	
	scalar N_Switchers3_`pairwise'`pla'XX = .
	scalar Nstayers3_`pairwise'`pla'XX = .
	
	cap drop used_in_IV`pairwise'_XX
	gen used_in_IV`pairwise'_XX =.
}
}
else{
///>put deltaD_t at the same level of D_(t-1) 
cap drop deltaD_temp
bysort ID_XX: gegen deltaD_temp = mean(deltaD_XX)
replace deltaD_XX = deltaD_temp
drop deltaD_temp 

if (`iwas'==1){
	///>put deltaZ_t at the same level of Z_(t-1)
	bysort ID_XX: gegen deltaZ_temp = mean(deltaZ_XX)
	replace deltaZ_XX = deltaZ_temp
	drop deltaZ_temp
	
	gen SI_XX = (deltaZ_XX>0)-(deltaZ_XX<0) // = SI+ - SI-, note that SI_XX = sgn(deltaZ_XX) which will be then used below
	rename  Z_XX Z1_XX
}

//First we need to tag observations that are used: this is related to the problem with unbalanced panel, and in a particular case I remarked when dealing with the aggregated influence functions
cap drop used_in_`pairwise'_XX
gen used_in_`pairwise'_XX = (deltaY_XX!=.&deltaD_XX!=.) //This variable will be used in aggreagation to set to . the influence function of the units that are not used in all the different two-periods we consider.
if (`iwas' == 1){
	cap drop used_in_IV`pairwise'_XX
	gen used_in_IV`pairwise'_XX = (used_in_`pairwise'_XX==1&deltaZ_XX!=.) 
	drop if !used_in_IV`pairwise'_XX 
}
else{
	//drop if !used_in_`pairwise'_XX //missing values: after the merge, below, with all the datasets that variable will be set to . for the unused units then we will exploit that to tag those units.(deprecated, I used rownonmiss instead)
}



//generate Switcher : S = 1 if switcher-up, -1 if switcher-down, 0 if stayer
gen S_XX = (deltaD_XX>0)-(deltaD_XX<0) // = S+ - S-

if (`was'==1|`as'==1){
cap drop absdeltaD_XX
gen absdeltaD_XX = S_XX*deltaD_XX

	if ("`switchers'" =="up"){
	 drop if S_XX==-1
	}
	if ("`switchers'" =="down"){
	 drop if S_XX==1
	}
}

if (`iwas'==1){
cap drop absdeltaZ_XX
gen absdeltaZ_XX = SI_XX*deltaZ_XX

	if ("`switchers'" =="up"){
	 drop if SI_XX==-1
	}
	if ("`switchers'" =="down"){
	 drop if SI_XX==1
	}
}


*******************************************************************************************
//Other treatments option 
*******************************************************************************************
if ("`other_treatments'"!=""){
cap drop group_other_treatments_XX
egen group_other_treatments_XX = group(`other_treatments')
	//1. Create the first difference of each treatment
	xtset ID_XX T_XX
	foreach var in `other_treatments'{
		cap drop fd_`var'_tmpXX 
		cap drop fd_`var'_XX 
		bysort ID_XX: gen fd_`var'_tmpXX = D.`var'
		//drop if the other treatment has changed. //We do not drop but set H_t to 0 afterward 
		bysort ID_XX: egen fd_`var'_XX = total(fd_`var'_tmpXX) 
		//drop if fd_`var'_XX !=0
		
	}
}
//We have all the variable we need at the first year so we can drop the 'second' year line
//  But before let's also take the right line of weightss we need: W_{g,t} if we consider the cell (t-1,t) (Effect), and even with Plaxebo

replace weights_XX = F.weights_XX
replace weights_cXX = F.weights_cXX

//Do it for the quantile to consider: 
	if (`quantile'>0){
		if (`by_fd'>1){ //FD:
			if (`as'==1|`was'==1)    replace Q_XX = F.Q_XX
			if (`iwas'==1)             replace QZ_XX = F.QZ_XX
		}
	}


if ("`placebo'"=="0"){
quietly drop if T_XX == 2 
}
else{ //Keep only line t-1.
if (`placebo'==1) quietly drop if T_XX == 3
if (`placebo'>1) quietly drop if T_XX == 4
}
rename  D_XX D1_XX

//+ Imbalanced panel adjustment : The missing value indicator: A 
cap drop Ht_XX
gen Ht_XX = (deltaD_XX!=.&deltaY_XX!=.)
replace S_XX =. if Ht_XX==0 // keep defined only for units with no missing data
if (`iwas'==1){
	replace Ht_XX = 1 if Ht_XX==1&deltaZ_XX!=.
	replace  SI_XX =. if Ht_XX==0
}
//If by quantile's options requested, We keep all stayers but only switchers such that Q_t = `quantile'
if (`quantile'>0 & (`by_fd'>1|`by_baseline'>1)){
if (`as'==1|`was'==1)  keep if (Q_XX==`quantile'&(S_XX==1|S_XX==-1))|(S_XX==0)
if (`iwas'==1) keep if (QZ_XX==`quantile'&(SI_XX==1|SI_XX==-1))|(SI_XX==0)
}
if (_N>0){ //In this section we use bysort ..: gen . If the dataset is empty (e.g, the user specified switchers(up) and there is no switcher-up for the current pair of times),  bysort ..: gen does not displays an error and does not create the variable, then if we call the variable (which was supposed to be created) the program crashes. To avoid that, I add the if  condition (_N>0) after the subsample based on the option switchers in line 1283.

***************************************************************************************/
if(`was'==1|`as'==1){
local vars_to_set_to_missing S_XX deltaD_XX deltaY_XX D1_XX absdeltaD_XX `cluster' `controls'	
}
else{
local vars_to_set_to_missing S_XX deltaD_XX deltaY_XX D1_XX `cluster' `controls'
}

/***********************************************/		

// + Imbalanced panel adjustements

	//1. Placebos
	if ("`placebo'"!="0"){
		//Very important to correct for both imbalanced panel and dropping switchers
	foreach var in `vars_to_set_to_missing'{
		cap replace `var' = .  if inSamplePlacebo_XX==0
	}
	replace Ht_XX = 0  if inSamplePlacebo_XX==0

			if (`iwas' ==1){
			replace Z1_XX = .  if inSamplePlacebo_XX==0	
			replace SI_XX = .  if inSamplePlacebo_XX==0	
			}
	}
	//2. Other treatments
	
	if ("`other_treatments'"!=""){
		foreach other_treat in `other_treatments'{
		//Very important to correct for both imbalanced panel and dropping switchers
			foreach var in `vars_to_set_to_missing'{
				cap replace `var' = .  if fd_`other_treat'_XX !=0
			}
			
			replace Ht_XX = 0  if fd_`other_treat'_XX !=0

					if (`iwas' ==1){
					replace Z1_XX = .  if fd_`other_treat'_XX !=0
					replace SI_XX = .  if fd_`other_treat'_XX !=0
					}
		}
	}	

***************************************************************************************/

if("`noextrapolation'"!=""){
	
	if (`as' ==1|`was'==1){
	sum D1_XX if S_XX==0
	scalar max_D1_`pla'XX = r(max)
	scalar min_D1_`pla'XX = r(min)
	gen outOfBounds_XX = (D1_XX<scalar(min_D1_`pla'XX)|D1_XX>scalar(max_D1_`pla'XX))
	count if outOfBounds_XX==1
	scalar N_drop_`pairwise'`pla'XX =  r(N)
	if (scalar(N_drop_`pairwise'`pla'XX)>0&scalar(gap_`pairwise'`pla'XX)==0&scalar(N_drop_`pairwise'`pla'XX)<_N-1){ //We count the number of dropped switches only if the point estimate can be computed.
	//di as error "No extrapolation:" N_drop_`pairwise'`pla'XX " switcher(s) dropped for t = `pairwise':" //test
	scalar N_drop_noextra_`pla'XX = scalar(N_drop_noextra_`pla'XX) + scalar(N_drop_`pairwise'`pla'XX)
	}
	drop if outOfBounds_XX==1
	}
	
	if(`iwas'==1){
	sum Z1_XX if SI_XX==0
	scalar max_Z1_`pla'XX = r(max)
	scalar min_Z1_`pla'XX = r(min)
	gen outOfBoundsiV_XX = (Z1_XX<scalar(min_Z1_`pla'XX)|Z1_XX>scalar(max_Z1_`pla'XX))
	
	count if outOfBoundsiV_XX==1
	scalar N_IVdrop_`pairwise'`pla'XX = r(N) //Just keep the number of switchers that violate the noextrapolation condition ; I display the nummber in the outputs of the command as a way to let the users know the consequences of the nonextra option.
	drop if outOfBoundsiV_XX==1
	if (scalar(N_IVdrop_`pairwise'`pla'XX)>0&scalar(gap_`pairwise'`pla'XX)==0&scalar(N_IVdrop_`pairwise'`pla'XX)<_N-1){
		scalar N_drop_noextra_`pla'XX = scalar(N_drop_noextra_`pla'XX) + scalar(N_IVdrop_`pairwise'`pla'XX)
	}
	}
}
if ("`exact_match'"!=""){
	cap drop s_has_match_temp_XX
	cap drop s_has_match_XX
	
	cap drop c_has_match_temp_XX
	cap drop c_has_match_XX
	
		if ("`other_treatments'"!=""){
			local search_match D1_XX group_other_treatments_XX // group_other_treatments_XX
		}
		else{
			local search_match D1_XX
		}
		if (`as' ==1|`was'==1){
		gegen s_has_match_temp_XX = min(absdeltaD_XX)  if S_XX!=. , by(`search_match')
		gen s_has_match_XX = (s_has_match_temp_XX==0)  if S_XX!=.
		
		gegen c_has_match_temp_XX = max(absdeltaD_XX)  if S_XX!=. , by(`search_match')
		gen c_has_match_XX = (c_has_match_temp_XX>0&c_has_match_temp_XX!=.)   if S_XX!=.
		
		replace s_has_match_XX=. if S_XX==0 
		replace c_has_match_XX=. if S_XX!=0&S_XX!=. 
		
			count if s_has_match_XX==0
	        scalar N_drop_`pairwise'`pla'XX = r(N)
			
			count if c_has_match_XX==0
	        scalar N_drop_c_`pairwise'`pla'XX = r(N)
		}

		if (`iwas' ==1){
			
		if ("`other_treatments'"!=""){
			local search_match Z1_XX group_other_treatments_XX
		}
		else{
			local search_match Z1_XX
		}

		gegen s_has_match_temp_XX = min(absdeltaZ_XX) if SI_XX!=. , by(`search_match')
		gen s_has_match_XX = (s_has_match_temp_XX==0) if SI_XX!=.
		
		gegen c_has_match_temp_XX = max(absdeltaZ_XX) if SI_XX!=., by(`search_match')
		gen c_has_match_XX = (c_has_match_temp_XX>0&c_has_match_temp_XX!=.)  if SI_XX!=.
		
		replace s_has_match_XX=. if SI_XX==0 
		replace c_has_match_XX=. if SI_XX!=0&SI_XX!=. 
		
			count if s_has_match_XX==0
	        scalar N_drop_`pairwise'`pla'XX = r(N)
			
			count if c_has_match_XX==0
	        scalar N_drop_c_`pairwise'`pla'XX = r(N)
		}
		
/***************************************************************************************/
	//drop if s_has_match_XX==0 //
	//drop if c_has_match_XX==0 //
	
	//Very important to correct for both imbalanced panel and dropping switchers
		
	foreach var in `vars_to_set_to_missing'{
		replace `var' = .  if s_has_match_XX==0|c_has_match_XX==0
	}

	replace Ht_XX = 0  if s_has_match_XX==0|c_has_match_XX==0
	
			if (`iwas' ==1){
			replace Z1_XX = .  if s_has_match_XX==0|c_has_match_XX==0		
			replace SI_XX = .  if s_has_match_XX==0|c_has_match_XX==0		
			}
***************************************************************************************/			
	if (scalar(N_drop_`pairwise'`pla'XX)>0&scalar(gap_`pairwise'`pla'XX)==0&scalar(N_drop_`pairwise'`pla'XX)!=_N){
	scalar N_drop_noextra_`pla'XX = scalar(N_drop_noextra_`pla'XX) + scalar(N_drop_`pairwise'`pla'XX)
	}
	
	if (scalar(N_drop_c_`pairwise'`pla'XX)>0&scalar(gap_`pairwise'`pla'XX)==0&scalar(N_drop_c_`pairwise'`pla'XX)!=_N){
	scalar N_drop_c_noextra_`pla'XX = scalar(N_drop_c_noextra_`pla'XX) + scalar(N_drop_c_`pairwise'`pla'XX)
	//di as error scalar(N_drop_c_`pairwise'`pla'XX)
	}

}

}
//+ Imbalanced panel adjustment : The missing value indicator: B
sum Ht_XX [iweight = weights_XX]
scalar PHt`pairwise'`pla'XX = r(mean) // Compute P(H_t=1) to be used in the adjustements

//di as error "`pairwise'`pla' : "scalar(PHt`pairwise'`pla'XX)

/*********************************
Some scalars we need below:
*********************************/
sum weights_XX
scalar W_`pla'XX = r(sum)
count if S_XX!=.
scalar N_`pla'XX = r(N)

if ("`cluster'"!=""){
	//Compute E(N_c)
cap drop N_c_XX
bysort `cluster': gen N_c_XX = _N if _n==1
cap sum N_c_XX
// MODIF FELIX NEW
scalar N_bar_c_`pairwise'`pla'XX = 0
if _rc==0{
	scalar N_bar_c_`pairwise'`pla'XX = r(mean)
}
//di as error N_bar_c_`pairwise'`pla'XX
}

*******Here I handle two related problems: Panel with gaps (using tsfilled_XX) and cases where we have only switchers or only stayers (using count)
if (`was'==1|`as'==1){
// only switchers or only stayers: I did add the index `pairwise' since I want to use it if either as or was is requested.

count if S_XX!=0&S_XX!=.
scalar n_switchers_`pla'XX = r(N) 

count if S_XX==0
scalar n_stayers_`pla'XX = r(N)


//Numbers to display*****************************************************
scalar Nstayers1_`pairwise'`pla'XX  = scalar(n_stayers_`pla'XX)
scalar Nstayers2_`pairwise'`pla'XX  = scalar(n_stayers_`pla'XX)

scalar N_Switchers1_`pairwise'`pla'XX = scalar(n_switchers_`pla'XX)
scalar N_Switchers2_`pairwise'`pla'XX = scalar(n_switchers_`pla'XX)

//Test weightsed numbers: to be added in the rest of the code.
sum weights_XX if S_XX==0
scalar Nstayers1_`pairwise'`pla'XX  = round(r(sum), 0.001)
scalar Nstayers2_`pairwise'`pla'XX  = round(r(sum), 0.001)

sum weights_XX if S_XX!=0&S_XX!=. 
scalar N_Switchers1_`pairwise'`pla'XX = round(r(sum), 0.001)
scalar N_Switchers2_`pairwise'`pla'XX = round(r(sum), 0.001)

}

if (`iwas'==1){
count if SI_XX!=0&SI_XX!=.
scalar n_switchersIV_`pla'XX = r(N) 

count if SI_XX==0
scalar n_stayersIV_`pla'XX = r(N)

scalar N_Switchers3_`pairwise'`pla'XX = scalar(n_switchersIV_`pla'XX)
scalar Nstayers3_`pairwise'`pla'XX  = scalar(n_stayersIV_`pla'XX)

sum weights_XX if SI_XX==0
scalar Nstayers3_`pairwise'`pla'XX  = round(r(sum), 0.001)

sum weights_XX if SI_XX!=0&SI_XX!=. 
scalar N_Switchers3_`pairwise'`pla'XX = round(r(sum), 0.001)



}
**************************************************************************	
	if ("`exact_match'"!=""){ //Take the number of distinct values of the baseline treatment: ok
		levelsof D1_XX
		local order = r(r)
	}

	//Option weights. For interaction and covariates terms in the regressions
	sum weights_XX
if (r(sd)!=0){
 	local controls `controls' `weights'
}


if (`was' == 1 | `as' == 1 ){	
if (scalar(gap_`pairwise'`pla'XX)==0&scalar(n_switchers_`pla'XX)>0&scalar(n_stayers_`pla'XX)>1){ //Start of feasible estimation //I Need to do it for the IV as well. 

**# Bookmark #0 Preliminaries
	cap drop predicted_XX
	cap drop mean_pred_XX
	cap drop trimmed_out_XX
	cap drop inner_sumdelta1_XX

    cap drop ESbis_XX_D1
	cap drop ES_XX_D1

/*******************************************************************
	Call polynomials_generator here: Start
	*******************************************************************/
if ("`cross_validation'" == ""){
polynomials_generator, order(`order')         prefix(reg)         controls(`controls') other_treatments(`other_treatments')	`pla'
	local reg_vars_pol_XX        "`s(reg_pol_XX)'"
	local logit_bis_pol_XX       "`s(reg_pol_XX)'"
	local logit_Plus_pol_XX      "`s(reg_pol_XX)'"
	local logit_Minus_pol_XX     "`s(reg_pol_XX)'"
}
else{
	
polynomials_generator, order(`reg_order')         prefix(reg)         controls(`controls') other_treatments(`other_treatments') `pla'
polynomials_generator, order(`logit_bis_order')   prefix(logit_bis)   controls(`controls') other_treatments(`other_treatments') `pla'
polynomials_generator, order(`logit_Plus_order')  prefix(logit_Plus)  controls(`controls') other_treatments(`other_treatments') `pla'
polynomials_generator, order(`logit_Minus_order') prefix(logit_Minus) controls(`controls') other_treatments(`other_treatments') `pla'

	local reg_vars_pol_XX        "`s(reg_pol_XX)'"
	local logit_bis_pol_XX       "`s(logit_bis_pol_XX)'"
	local logit_Plus_pol_XX      "`s(logit_Plus_pol_XX)'"
	local logit_Minus_pol_XX     "`s(logit_Minus_pol_XX)'"
}

	/*******************************************************************
	Call polynomials_generator here: End
******************************************************************************/	

				//Generating the binary S
				cap drop Sbis_XX
				cap drop SbisV_XX
				gen Sbis_XX = (S_XX!=0) if S_XX!=. //this Sbis_XX is to be used for \delta_1, and for \delta_2 when we do not need to distinguish switchers-up from switchers-down where the only matter is being switcher or not.
				gen SbisV_XX = Sbis_XX*weights_XX
	
/*******************************************************************************
Perfom here the main logit regressions that are needed for the three estimators
*******************************************************************************/

	//*********************for AS, WAS

	// Performing the regression (polynomial series) estimation to estimate \hat{E}(deltaY|D1, S=0)
	
	     // \hat{E}(deltaY|D1, S=0): actually it is \hat{E}(deltaY|D1, S=0, H_t=1) since S_XX is only defined among {i: Hit=1}, all the expectations/probabilities that follow are conditioned on Ht=1.
		 
	reg deltaY_XX `reg_vars_pol_XX'   if S_XX==0 //This regression can generated an error when the polynomial order is very high and lead to values >  1e+38 (the upper bound of float in stata.).
	predict mean_pred_XX , xb 
	
		 // deltaY_i - \hat{E}(deltaY|D_{1i}|S = 0)
	gen inner_sumdelta12_XX  = deltaY_XX - mean_pred_XX //WILL BE USED FOR AS AND WAS AS WELL

// 1. Estimate P(S = 0|D_1) 
	cap drop S0_XX
	gen S0_XX = 1-Sbis_XX
    if ("`exact_match'"==""){
	capture logit S0_XX `logit_bis_pol_XX'   , asis
	if (_rc==430){
	//di as error "Warning: convergence not achieved." //To be dropped after?
	}
	predict PS0D1_XX, pr asif
	//Convention Logit STATA R to match
	replace PS0D1_XX=0 if PS0D1_XX<=10^(-10)
	}
	else{ 		//This is for the IF and point estimate of the was when we have discrete treatment, instead of using logit use regressions
	
		//Estimation of 1-E(S|D1) = P(S = 0|D_1) in exact_match case
		reg Sbis_XX `reg_vars_pol_XX'   
		predict ESbis_XX_D1, xb
		
		//Estimation of  \hat{E}(S+-S-|D1) for both \Phi_2 : Remember S_XX = S+ - S-, i.e (deltaD_XX>0)-(deltaD_XX<0) in line 79,  so do:
		reg S_XX `reg_vars_pol_XX'   
		predict ES_XX_D1 , xb  

	}

	//2. Estimate P(S=0)
	sum S0_XX 
	scalar PS0_`pla'XX = r(mean) //scalar(Nstayers2_`pairwise'XX )/(scalar(N_Switchers2_`pairwise'XX)+ scalar(Nstayers2_`pairwise'XX))
	
	//3. P(S+=1|D_1), P(S-=1|D_1), P(S+=1) and P(S-=1) are generated after.

	//*********************for iWAS: It is done in Bookmark #3
	

********************************************************************************
**# Bookmark #1 AS
********************************************************************************

if (`as' == 1){
	
************************************************	
	cap drop S_over_deltaD_XX
	cap drop meanS_over_deltaD_XX
	
	cap drop Phi1_`pairwise'`pla'XX //This influence function Phi1_{1,t}
************************************************	
	// 0) Compute P_t = P(S_t = 1) = E(S_t) for the aggregation afterward
	
	// + Imbalanced panel adjustment
	sum SbisV_XX //sum Sbis_XX changed because of weightss 
	//For aggregation
	scalar P_`pairwise'`pla'XX = r(mean) // This is actually P(S_t = 1|H_t=1) (resp. E(SV|H_t=1)), we need to adjust with PHt`pairwise'`pla'XX to have  P(S_t = 1, H_t=1) (resp. E(SVH_t))
	scalar P_`pairwise'`pla'XX = scalar(P_`pairwise'`pla'XX)*scalar(PHt`pairwise'`pla'XX)
	
	scalar PS_sum_`pla'XX = scalar(PS_sum_`pla'XX) + scalar(P_`pairwise'`pla'XX) //PS_sum is initialized outside of this program
	scalar ES_`pla'XX = r(mean)  // We fo not do the adjustment here, but will do it directly in the expression of the IF.

	// 1) Compute \hat{delta}_1
	gen inner_sumdelta1_XX  = weights_XX*inner_sumdelta12_XX/deltaD_XX // =SV*(\DeltaY - E(\DeltaY|S=0, D1, V))/\DeltaD
	replace inner_sumdelta1_XX = 0 if deltaD_XX==0 //The convention 0*missing = 0.
	sum inner_sumdelta1_XX 
	scalar delta1_`pairwise'`pla'XX = r(mean)/scalar(ES_`pla'XX) // = E[SV*(\DeltaY - E(\DeltaY|S=0, D1, V))/\DeltaD]/E[SV]

/**************************************************************************
          2. COMPUTING THE VARIANCE  of \hat{delta}_1
**************************************************************************/
	gen S_over_deltaD_XX = Sbis_XX/deltaD_XX // = V/\DeltaD
	//The convention 0/0 = 0
	replace S_over_deltaD_XX =0 if Sbis_XX==0
	
		//i. estimation of  \hat{E}(S/deltaD|D1)
	reg S_over_deltaD_XX `reg_vars_pol_XX'   
	predict meanS_over_deltaD_XX , xb  
	
	//Doulo: The Influence function is indexed by t to ease the aggregation after the loop is over by using successive merging of datasets
	
	if ("`exact_match'"==""){
	gen Phi1_`pairwise'`pla'XX  = weights_XX*(S_over_deltaD_XX - meanS_over_deltaD_XX*(1-Sbis_XX)/(PS0D1_XX))*inner_sumdelta12_XX //Here we use the prediction from a logit, if not exact_match
	}
	else{
	gen Phi1_`pairwise'`pla'XX  = weights_XX*(S_over_deltaD_XX - meanS_over_deltaD_XX*(1-Sbis_XX)/(1-ESbis_XX_D1))*inner_sumdelta12_XX //Here we use the prediction from a linear regression, if exact_match
	}
	// + Imbalanced panel adjustment:
		//a.  Deviding by scalar(PHt`pairwise'`pla'XX)
	replace Phi1_`pairwise'`pla'XX  = [Phi1_`pairwise'`pla'XX - scalar(delta1_`pairwise'`pla'XX)*Sbis_XX]/[scalar(ES_`pla'XX)*scalar(PHt`pairwise'`pla'XX)]
		//b. Replace the IF by 0 if Ht_XX==0
	replace Phi1_`pairwise'`pla'XX = 0 if Ht_XX==0
	
	sum Phi1_`pairwise'`pla'XX 
	
	if ("`cluster'"!=""){ // Clustering the variance //CLUSTER OPTION //weights OPTION
		cap drop Phi1_`pairwise'`pla'_cXX //I create a new variable, because I need the one without clustering in the aggregation
		bysort `cluster': egen Phi1_`pairwise'`pla'_cXX = total(Phi1_`pairwise'`pla'XX)
		bysort `cluster': replace Phi1_`pairwise'`pla'_cXX=. if _n!=1
		replace Phi1_`pairwise'`pla'_cXX = Phi1_`pairwise'`pla'_cXX/scalar(N_bar_c_`pairwise'`pla'XX)
		sum Phi1_`pairwise'`pla'_cXX	
	}

	scalar sd_delta1_`pairwise'`pla'XX = r(sd)/sqrt(r(sum_w))  //sqrt(r(N)) same
	
	scalar LB1_`pairwise'`pla'XX = scalar(delta1_`pairwise'`pla'XX) - 1.96*scalar(sd_delta1_`pairwise'`pla'XX)
	scalar UB1_`pairwise'`pla'XX = scalar(delta1_`pairwise'`pla'XX) + 1.96*scalar(sd_delta1_`pairwise'`pla'XX)
	
	//Now Let's store S_t*V_t for the Influence function variable of the aggregation
	cap drop S_`pairwise'`pla'XX
	gen S_`pairwise'`pla'XX = SbisV_XX
	
	// + Imbalanced panel adjustment: replace S_t by S_t*H_t in the aggreagation of the point estimates formula
	replace  S_`pairwise'`pla'XX = Ht_XX if Ht_XX==0 //(=S_t*H_t)
	replace  S_`pairwise'`pla'XX = S_`pairwise'`pla'XX*weights_XX //S_t*V_t
}

********************************************************************************
**# Bookmark #2 WAS
********************************************************************************
if (`was' == 1){
cap drop absdeltaDV_XX
gen absdeltaDV_XX = absdeltaD_XX*weights_XX //sgn(\DeltaD)*V
	sum absdeltaDV_XX //absdeltaD_XX
	scalar EabsdeltaD_`pla'XX = r(mean)
	//For aggregation of the point estimates
	scalar EabsdeltaD_`pairwise'`pla'XX =  r(mean) 
	//+ Imbalanced panel data: This is actually E(V|deltaD||H_t=1), we need to adjust with PHt`pairwise'`pla'XX to have  E(V|deltaD|, H_t=1) 
	scalar EabsdeltaD_`pairwise'`pla'XX = scalar(EabsdeltaD_`pairwise'`pla'XX )*scalar(PHt`pairwise'`pla'XX)
	
	scalar EabsdeltaD_sum_`pla'XX = scalar(EabsdeltaD_sum_`pla'XX) + scalar(EabsdeltaD_`pairwise'`pla'XX) //EabsdeltaD_sum_XX is initialized outside of this program
	
	cap drop Phi2_`pairwise'`pla'XX //This influence function Phi2_{1,t}

/**************************************************************************
            1. COMPUTING THE POINT ESTIMATE \hat{delta}_2: START
**************************************************************************/
	foreach suffix in  "Minus" "Plus"{
	
		//i)compute \hat{delta}_2+ and \hat{delta}_2-, and w+

		if ("`suffix'" =="Plus"){
			cap drop Ster_XX
			gen Ster_XX = (S_XX==1) 
			//replace Ster_XX =. if S_XX==-1
		}
		if ("`suffix'" =="Minus") {
			cap drop Ster_XX
			gen Ster_XX = (S_XX==-1)
			//replace Ster_XX =. if S_XX==1
		}
		
/**************************************************************************
       i. COMPUTING THE contribution-weightsS (wPlus and wMinus)
**************************************************************************/
		cap drop prod_sgndeltaDdeltaD_XX
		gen prod_sgndeltaDdeltaD_XX = S_XX*deltaD_XX
		sum prod_sgndeltaDdeltaD_XX if Ster_XX==1
		//scalar w`suffix'_`pairwise'`pla'XX = r(sum)/scalar(N_`pla'XX)
		scalar w`suffix'_`pairwise'`pla'XX = r(sum)/scalar(N_`pla'XX) //scalar(W_`pla'XX) // because of the weights option 
//The sum at the denominator of \hat{\delta}_2`suffix'

		//use r(sum) instead of r(mean) since in case there is no observation r(mean) gives ., wheras r(sum) will give 0: this is for the r-based approach.
		cap drop deltaDV_XX
		gen deltaDV_XX = deltaD_XX*weights_XX
		sum deltaDV_XX  if Ster_XX==1
		scalar denom_delta2`suffix'_`pairwise'`pla'XX = r(sum)
		

/**************************************************************************
		           IF REGRESSION-BASED APPROACH 
**************************************************************************/
		if ("`estimation_method'" == ""|"`estimation_method'" == "ra"){
		
		if (denom_delta2`suffix'_`pairwise'`pla'XX ==0){
			scalar denom_delta2`suffix'_`pairwise'`pla'XX = 1 //in case it is zero set it to 1 to avoid dividing by 0, in that case the numerator is also equal to 0
		}
		cap drop Vinner_sumdelta12_XX
		gen Vinner_sumdelta12_XX = inner_sumdelta12_XX*weights_XX
		sum  Vinner_sumdelta12_XX  if Ster_XX==1
		scalar num_delta2`suffix'_`pairwise'`pla'XX = r(sum)
		
		scalar delta2`suffix'_`pairwise'`pla'XX = scalar(num_delta2`suffix'_`pairwise'`pla'XX)/scalar(denom_delta2`suffix'_`pairwise'`pla'XX) // = E[V(\DeltaD){\DeltaY - E(\DeltaY|S=0, D1,  V)}]/E(V\DeltaD)
		}
		
/**********************************************************************************
	  o. Preliminaries: All the variables needed regardless the estimation_method
**********************************************************************************/
	       cap drop  PS1`suffix'D1_XX
		   
	       //1. Estimate P(S`suffix'=1)
		   count if Ster_XX ==1
		   scalar nb_Switchers`suffix'`pla'XX = r(N) // I need the non-weightsed number of switchers to display
		   
		   scalar PS`suffix'1`pla'XX = scalar(nb_Switchers`suffix'`pla'XX)/scalar(N_`pla'XX)
	
		   
if ("`exact_match'"==""){ //We only do the logit regression if we have continuous treatment, i.e., when exact_match is not specified	
		   if (scalar(PS`suffix'1`pla'XX)==0){ //I do the regression iff there is at least one switcher up/down.
		   scalar delta2`suffix'_`pairwise'`pla'XX  = 0 // the weightss as well -see above-is set to 0 if {i: Ster_XX==1} = empty.
		   
		   cap drop PS1`suffix'D1_XX //create it and set it to zero, since I will call it outside this loop for the dr method.
		   gen PS1`suffix'D1_XX = 0
		   }
		   else{ 
		   	//2. Estimate P(S`suffix' = 1|D_1) 
			capture logit Ster_XX `logit_`suffix'_pol_XX'     , asis
			if (_rc==430){
				//di as error "Warning: convergence not achieved."
			}
			
			predict PS1`suffix'D1_XX , pr asif
			//Convention Logit STATA R
			replace PS1`suffix'D1_XX=0 if PS1`suffix'D1_XX<=10^(-10)
			
		
/**************************************************************************
			               IF PS-based APPROACH
**************************************************************************/
		if ("`estimation_method'" == "ps"){
			//3. Compute deltaY*[(PS1`suffix'D1_XX*PS0_XX)/(PS0D1_XX*PS`suffix'1XX)]
			cap drop deltaYP_`suffix'XX
			gen deltaYP_`suffix'XX = weights_XX*deltaY_XX*(PS1`suffix'D1_XX/PS0D1_XX)*(scalar(PS0_`pla'XX)/scalar(PS`suffix'1XX))
			sum deltaYP_`suffix'XX if S_XX==0
			scalar mean_deltaYP_`suffix'`pla'XX = r(mean)
			
			cap drop deltaYV_XX
			gen deltaYV_XX = deltaY_XX*weights_XX
			sum deltaY_XX  if Ster_XX ==1
			scalar mean_deltaY_`pla'XX = r(mean) //E(\DeltaY V)
			
			sum deltaDV_XX  if Ster_XX==1
			
			scalar delta2`suffix'_`pairwise'`pla'XX = (scalar(mean_deltaY_`pla'XX) - scalar(mean_deltaYP_`suffix'`pla'XX))/r(mean)
		}
		} //End of the else
} //End of the if with exact_match
	} //End of the suffix loop

/**************************************************************************
      ii. COMPUTING THE FINAL weightsS WPLus = wPlus/(wPlus+wMinus)
**************************************************************************/
if ("`estimation_method'" == ""|"`estimation_method'" == "ps"|"`estimation_method'" == "ra"){
	scalar W_Plus_`pairwise'`pla'XX = scalar(wPlus_`pairwise'`pla'XX)/(scalar(wPlus_`pairwise'`pla'XX)+scalar(wMinus_`pairwise'`pla'XX))
}
	
	
/**************************************************************************
                       COMPUTING THE POINT ESTIMATE: END
**************************************************************************/
if ("`exact_match'"==""){
	    cap drop dr_deltaYV_XX
	    gen dr_deltaYV_XX = weights_XX*(S_XX - [(PS1PlusD1_XX - PS1MinusD1_XX)/PS0D1_XX]*(1-Sbis_XX))*inner_sumdelta12_XX if Sbis_XX == 0
		
		replace dr_deltaYV_XX = weights_XX*(S_XX)*inner_sumdelta12_XX if Sbis_XX == 1 // Modif Doulo: RF or FS giving 0.
		
		sum dr_deltaYV_XX  //WIll use it for the dr point estimate and for the estimation of the variance!
		scalar num_dr_delta2_`pla'XX = r(sum)
}		
		if ("`estimation_method'" == ""|"`estimation_method'" == "ps"|"`estimation_method'" == "ra"){
		scalar delta2_`pairwise'`pla'XX = scalar(W_Plus_`pairwise'`pla'XX)*scalar(delta2Plus_`pairwise'`pla'XX) + (1 - scalar(W_Plus_`pairwise'`pla'XX))*scalar(delta2Minus_`pairwise'`pla'XX ) 
		}
	
		if ("`estimation_method'" == "dr"){
		sum absdeltaDV_XX 
		scalar delta2_`pairwise'`pla'XX = scalar(num_dr_delta2_`pla'XX)/r(sum)
		}
	
/**************************************************************************
          2. COMPUTING THE VARIANCE (The variance is not method-specific) // but we use linear regression if exact_match, and logit otherwise
**************************************************************************/
        if ("`exact_match'"==""){
		gen Phi2_`pairwise'`pla'XX = (dr_deltaYV_XX -scalar(delta2_`pairwise'`pla'XX)*absdeltaDV_XX)
		}
		else{
		gen Phi2_`pairwise'`pla'XX = weights_XX*[(S_XX - ES_XX_D1*(1-Sbis_XX)/(1-ESbis_XX_D1))*inner_sumdelta12_XX -scalar(delta2_`pairwise'`pla'XX)*absdeltaD_XX]
		}
		// + Imbalanced panel adjustment: Adjust the IF
			//a. Adjust the denominator of the IF
		replace Phi2_`pairwise'`pla'XX = Phi2_`pairwise'`pla'XX/[scalar(PHt`pairwise'`pla'XX)*scalar(EabsdeltaD_`pla'XX)]
			//b. Replace the IF by 0 if Ht_XX==0
		replace Phi2_`pairwise'`pla'XX = 0 if Ht_XX==0	
		
		
		//save "dr_`pairwise'.dta", replace
		sum  Phi2_`pairwise'`pla'XX 

		if ("`cluster'"!=""){ // Clustering the variance
		cap drop Phi2_`pairwise'`pla'_cXX //I create a new variable, because I need the one without clustering in the aggregation
		bysort `cluster': egen Phi2_`pairwise'`pla'_cXX = total(Phi2_`pairwise'`pla'XX)
		bysort `cluster': replace Phi2_`pairwise'`pla'_cXX=. if _n!=1
		replace Phi2_`pairwise'`pla'_cXX = Phi2_`pairwise'`pla'_cXX/scalar(N_bar_c_`pairwise'`pla'XX)
		sum Phi2_`pairwise'`pla'_cXX   
		}
	
		scalar sd_delta2_`pairwise'`pla'XX = r(sd)/sqrt(r(sum_w)) // sqrt(r(sum_w)) instead of r(N) same
		scalar LB2_`pairwise'`pla'XX = scalar(delta2_`pairwise'`pla'XX) - 1.96*scalar(sd_delta2_`pairwise'`pla'XX)
		scalar UB2_`pairwise'`pla'XX = scalar(delta2_`pairwise'`pla'XX) + 1.96*scalar(sd_delta2_`pairwise'`pla'XX)
		
		//Now Let's store absdeltaD_t for the Influence function variable of the aggregated point estimate
		cap drop absdeltaD_`pairwise'`pla'XX
		gen absdeltaD_`pairwise'`pla'XX = absdeltaD_XX
		// + Imbalanced panel adjustment: replace |deltaD_t| by |deltaD_t|*H_t in the aggreagation of the point estimates formula
		replace absdeltaD_`pairwise'`pla'XX  = 0 if Ht_XX == 0
		replace absdeltaD_`pairwise'`pla'XX =  absdeltaD_`pairwise'`pla'XX*weights_XX //AbsDelta_t*V_t
}
}
//End of non-IV feasible estimation
else{
	forvalues i=1/2{
	scalar delta`i'_`pairwise'`pla'XX  = . //0
	scalar sd_delta`i'_`pairwise'`pla'XX = .
	scalar LB`i'_`pairwise'`pla'XX = .
	scalar UB`i'_`pairwise'`pla'XX = .
	gen Phi`i'_`pairwise'`pla'XX = .
	
	//If we are in gaps cases
		if (scalar(gap_`pairwise'`pla'XX) !=0){
		scalar N_Switchers`i'_`pairwise'`pla'XX = .
		scalar Nstayers`i'_`pairwise'`pla'XX = .
		}
		else{
	//If we are in only one type cases
		if (scalar(n_stayers_XX)<2){
		scalar N_Switchers`i'_`pairwise'`pla'XX = _N - scalar(n_stayers_XX)
		scalar Nstayers`i'_`pairwise'`pla'XX = scalar(n_stayers_XX)
		}	
		
		if (scalar(n_switchers_XX)==0){
		scalar N_Switchers`i'_`pairwise'`pla'XX = 0
		scalar Nstayers`i'_`pairwise'`pla'XX = _N
		}	
	}
	}
	gen absdeltaD_`pairwise'`pla'XX = .
	gen S_`pairwise'`pla'XX = .
	scalar EabsdeltaD_`pairwise'`pla'XX = 0
	scalar P_`pairwise'`pla'XX = 0
}
}
********************************************************************************
**# Bookmark #3 IWAS
********************************************************************************
if (`iwas' == 1){
if (scalar(gap_`pairwise'`pla'XX)==0&scalar(n_switchersIV_`pla'XX)>0&scalar(n_stayersIV_`pla'XX)>1){ //Start of IV feasible estimation
************************************************

		//polynomial for Z if IV requested
		
			local varsIV_pol_XX = "`other_treatments'"
			forvalues pol_level = 1/`order'{
			scalar pol_level_`pla'XX = `pol_level'
			capture drop Z1_XX_`pol_level'_XX 
			gen Z1_XX_`pol_level'_XX = Z1_XX^scalar(pol_level_`pla'XX)
			local varsIV_pol_XX = "`varsIV_pol_XX' Z1_XX_`pol_level'_XX"
			
			//Controls: Generate the polynomial order of each control
			
			if ("`controls'"!=""){
				foreach control in `controls'{
					cap drop `control'_`pol_level'_XX
					gen `control'_`pol_level'_XX = `control'^scalar(pol_level_`pla'XX)
					local vars_pol_controlsXX = "`vars_pol_controlsXX' `control'_`pol_level'_XX"
				}
			}
			}
					//Add the interaction control#Z1_XX
			if ("`controls'"!=""&`order'>1){
				foreach control in `controls'{
					local vars_pol_controlsXX = "`vars_pol_controlsXX' c.`control'#c.Z1_XX"
				}
			}
			//Add the interaction control#control only zhen order>=2
			if (`order'>1){
			if ("`controls'"!=""){
				local i = 0
				foreach control1 in `controls'{
					local i = `i'+1
					local j = 0
					foreach control2 in `controls'{
						local j = `j'+1
						if (`j'>`i') {
							local vars_pol_controlsXX = "`vars_pol_controlsXX' c.`control1'#c.`control2'"
						}
					}
				}
			}	
			}
			//// Other treatments option: More general and inclusive way of doing the interaction.
			
			if ("`other_treatments'"!=""){
				foreach var in `other_treatments'{
					if ("`interact'"==""){
						local interact  = "c.`var'##c.Z1_XX"
					}
					else{
						local interact  = "c.`interact'##c.`var'"
					}
			local varsIV_pol_XX = "`varsIV_pol_XX' `interact'"
				}
			}
			//All controls
			local varsIV_pol_XX = "`varsIV_pol_XX' `vars_pol_controlsXX'"
			//di as error "`varsIV_pol_XX'"
		
cap drop innerSumIV_num_XX
//cap drop absdeltaZ_XX

cap drop SIbis_XX
cap drop SIPlus_XX
cap drop SIMinus_XX

	//gen absdeltaZ_XX = SI_XX*deltaZ_XX
	sum absdeltaZ_XX 
	scalar EabsdeltaZ_`pairwise'`pla'XX = r(mean)
	
    gen SIbis_XX = (SI_XX!=0&SI_XX!=.) 
	gen SIPlus_XX = (SI_XX==1)
	gen SIMinus_XX = (SI_XX==-1)
	
cap drop Phi_Y_XX
cap drop Phi_D_XX
cap drop Phi3_`pairwise'`pla'XX


/************************************************	
o. Preliminaries: Perform the logit regressions
************************************************/

	// 1. Estimate P(SI = 0|Z_1) 
	cap drop S_IV0_XX
	gen S_IV0_XX = 1-SIbis_XX
if ("`exact_match'"==""){
	cap logit S_IV0_XX `varsIV_pol_XX'    , asis
	if (_rc==430){
				//di as error "Warning: convergence not achieved."
			}
	predict PS_IV0Z1_XX , pr asif
	//Convention Logit STATA R
	replace PS_IV0Z1_XX=0 if PS_IV0Z1_XX<=10^(-10)
}
else{ 		//This is for the IF and point estimate of the was when we have discrete treatment, instead of using logit use regressions
	
		//Estimation of 1-E(SI|Z1) = P(SI = 0|Z_1) in exact_match case
		reg SIbis_XX `varsIV_pol_XX'   
		predict ESIbis_XX_Z1, xb
		//Estimation of  \hat{E}(SI+-SI-|Z1) 
		reg SI_XX `varsIV_pol_XX'   
		predict ESI_XX_Z1 , xb  
	}	
	//2. P(SI=0) //For PS and DR-BASED APPROACHES
	sum S_IV0_XX 
	scalar PS_IV0_`pla'XX = r(mean)
	
	//3. P(SI+=1|Z_1), P(SI-=1|Z_1) (For \hat{E}(SI+-SI-|Z1)), P(SI+=1) and P(SI-=1) //For the influence function, PS and DR

       cap drop  PSIPlus1Z1_XX
	   cap drop  PSIMinus1Z1_XX
		   
	   foreach suffix in  "Minus" "Plus"{
	   	cap drop  PSI`suffix'1Z1_XX
	       //1. Estimate P(SI`suffix'=1)
		   count if SI`suffix'_XX ==1
		   scalar nb_SwitchersI`suffix'`pla'XX = r(N) 
		   scalar PSI`suffix'1`pla'XX = scalar(nb_SwitchersI`suffix'`pla'XX)/scalar(N_`pla'XX)	 //P(SI+=1) and P(SI-=1) 
		   

		   
		   
		   if (scalar(PSI`suffix'1`pla'XX)==0){ //I do the regression iff there is at least one switcher up/down.
		   		   
             //create it and set it to zero, since I will call it outside this loop for the dr method.
		   gen PSI`suffix'1Z1_XX = 0
		   }
		   else{ 
			if ("`exact_match'"==""){
			
 capture logit SI`suffix'_XX `varsIV_pol_XX'     , asis
			if (_rc==430){
				//di as error "Warning: convergence not achieved."
			}
			
			predict PSI`suffix'1Z1_XX , pr asif //P(SI+=1|Z_1) and P(SI-=1|Z_1)
			//Convention Logit STATA R
			
			replace PSI`suffix'1Z1_XX=0 if PSI`suffix'1Z1_XX<=10^(-10)
		   } //end of if exact_match
		   }
	   }
/**************************************************************************
            1. COMPUTING THE POINT ESTIMATE \hat{delta}_IV: START
**************************************************************************/
 	//i. Estimation of  \hat{E}(deltaY|Z1, SI=0)
	cap drop meandeltaY_predIV_XX
	reg deltaY_XX `varsIV_pol_XX'     if SI_XX==0
	predict meandeltaY_predIV_XX , xb 
	//scalar Nstayers3_`pairwise'XX  = e(N) 
	
	cap drop innerSumIV_num_XX
	gen innerSumIV_num_XX = deltaY_XX - meandeltaY_predIV_XX
	
	//ii. Estimation of \hat{E}(deltaD|Z1, SI=0)
	cap drop meandeltaD_predIV_XX
	reg deltaD_XX `varsIV_pol_XX'      if SI_XX==0
	predict meandeltaD_predIV_XX , xb 
	
	cap drop innerSumIV_denom_`pairwise'`pla'XX
	gen innerSumIV_denom_`pairwise'`pla'XX = deltaD_XX - meandeltaD_predIV_XX
	
/**************************************************************************
		           1.1 IF REGRESSION-BASED APPROACH 
**************************************************************************/
if ("`estimation_method'" == ""|"`estimation_method'" == "ra"){
	
	replace innerSumIV_num_XX = SI_XX*innerSumIV_num_XX
	sum innerSumIV_num_XX 
	scalar num_deltaIV`pairwise'`pla'XX = r(mean)
 
    replace innerSumIV_denom_`pairwise'`pla'XX = SI_XX*innerSumIV_denom_`pairwise'`pla'XX
	sum innerSumIV_denom_`pairwise'`pla'XX 
	scalar denom_deltaIV`pairwise'`pla'XX = r(mean)
 }

/**************************************************************************
		           1.2 IF PS or DR-BASED APPROACH 
**************************************************************************/
	
			if ("`estimation_method'" == "ps"){
				
			//1. The numerator	
			
			 //1.1. Compute deltaY*[(PSIPlus1Z1_XX-PSIMinus1Z1_XX)*PS_IV0_XX)/(PS_IV0Z1_XX)]
			 cap drop deltaYP_IVXX
			 gen deltaYP_IVXX = deltaY_XX*[(PSIPlus1Z1_XX-PSIMinus1Z1_XX)/PS_IV0Z1_XX*(scalar(PS_IV0_`pla'XX))]
			 sum deltaYP_IVXX  if SIbis_XX==0
			 scalar mean_deltaYP_IV`pla'XX = r(mean)
			 
			 //1.2 Compute sgn(deltaZ)*deltaY = SI_XX*deltaY_XX: Remember SI_XX = (deltaZ_XX>0) - (deltaZ_XX<0)
			 cap drop prod_sgndeltaZdeltaY_XX
			 gen prod_sgndeltaZdeltaY_XX = SI_XX*deltaY_XX
			 sum prod_sgndeltaZdeltaY_XX 
			 scalar mean_sgndeltaZdeltaY_`pla'XX = r(mean)
			 
			 //1.3
			 scalar num_deltaIV`pairwise'`pla'XX = scalar(mean_sgndeltaZdeltaY_`pla'XX) - scalar(mean_deltaYP_IV`pla'XX)

			//2. The denominator	
			
		     //2.1. Compute deltaD*[(PSIPlus1Z1_XX-PSIMinus1Z1_XX)*PS_IV0_XX)/(PS_IV0Z1_XX)]
			 cap drop deltaDP_IVXX
			 gen deltaDP_IVXX = deltaD_XX*[(PSIPlus1Z1_XX-PSIMinus1Z1_XX)/PS_IV0Z1_XX*(scalar(PS_IV0_`pla'XX))]
			 sum deltaDP_IVXX  if SIbis_XX==0
			 scalar mean_deltaDP_IV`pla'XX = r(mean)	
			 
			//2.2 Compute sgn(deltaZ)*deltaD = SI_XX*deltaD_XX
			 cap drop prod_sgndeltaZdeltaD_XX
			 gen prod_sgndeltaZdeltaD_XX = SI_XX*deltaD_XX
			 sum prod_sgndeltaZdeltaD_XX 
			 scalar mean_sgndeltaZdeltaD_`pla'XX = r(mean)	
			 
		    //2.3
			scalar denom_deltaIV`pairwise'`pla'XX = scalar(mean_sgndeltaZdeltaD_`pla'XX) - scalar(mean_deltaDP_IV`pla'XX)
			}
		
		if ("`estimation_method'" == "dr"){
			
			//1. The numerator	
			cap drop dr_IVdeltaY_XX
	        gen dr_IVdeltaY_XX = (SI_XX - [(PSIPlus1Z1_XX - PSIMinus1Z1_XX)/PS_IV0Z1_XX]*(1-SIbis_XX))*innerSumIV_num_XX if SIbis_XX == 0
			replace dr_IVdeltaY_XX = (SI_XX)*innerSumIV_num_XX if SIbis_XX == 1
			
			sum dr_IVdeltaY_XX 
			scalar num_deltaIV`pairwise'`pla'XX = r(mean) 

			//2. The denominator	
			cap drop dr_IVdeltaD_XX
	        gen dr_IVdeltaD_XX = (SI_XX - [(PSIPlus1Z1_XX - PSIMinus1Z1_XX)/PS_IV0Z1_XX]*(1-SIbis_XX))*innerSumIV_denom_`pairwise'`pla'XX if SIbis_XX == 0
			replace  dr_IVdeltaD_XX = (SI_XX)*innerSumIV_denom_`pairwise'`pla'XX if SIbis_XX == 1
			
			sum dr_IVdeltaD_XX 
			scalar denom_deltaIV`pairwise'`pla'XX = r(mean)
	
		}

 
 //Compute the point estimate by scalar(num_deltaIV)/scalar(denom_deltaIV)
 scalar delta3_`pairwise'`pla'XX = scalar(num_deltaIV`pairwise'`pla'XX)/scalar(denom_deltaIV`pairwise'`pla'XX)
 //For aggreagation + correcting imbalance panels
 scalar denom_deltaIV`pairwise'`pla'XX  = scalar(denom_deltaIV`pairwise'`pla'XX)*scalar(PHt`pairwise'`pla'XX)
 scalar denom_deltaIV_sum_`pla'XX = scalar(denom_deltaIV_sum_`pla'XX ) + scalar(denom_deltaIV`pairwise'`pla'XX) //denom_deltaIV_sum_`pla'XX is initialized outside of this program
 replace innerSumIV_denom_`pairwise'`pla'XX = innerSumIV_denom_`pairwise'`pla'XX*weights_XX //\hat{\pi}_{i,t}V_t
 replace innerSumIV_denom_`pairwise'`pla'XX = 0  if Ht_XX == 0
 /**************************************************************************
            2. COMPUTING the variance of \hat{delta}_IV: START
**************************************************************************/
	
	 //i. Compute phi_Y
	 sum innerSumIV_num_XX 
	 scalar delta_Y_`pairwise'`pla'XX = r(mean)
	 //scalar delta_Y_`pairwise'`pla'XX = r(mean)/scalar(EabsdeltaZ_`pairwise'`pla'XX)

	 //reg deltaY_XX `vars_pol_XX' if SI_XX==0 
	 reg deltaY_XX `varsIV_pol_XX'     if SI_XX==0 // meandeltaY_predIV_XX
	 predict mean_pred_Y_IV_XX , xb 
	 
	 if ("`exact_match'"==""){
	 gen Phi_Y_XX = (SI_XX - (PSIPlus1Z1_XX-PSIMinus1Z1_XX)*(1-SIbis_XX)/(PS_IV0Z1_XX))*(deltaY_XX - mean_pred_Y_IV_XX) - scalar(delta_Y_`pairwise'`pla'XX)*absdeltaZ_XX
	 }
	 else{
	 gen Phi_Y_XX = (SI_XX - (ESI_XX_Z1)*(1-SIbis_XX)/(1-ESIbis_XX_Z1))*(deltaY_XX - mean_pred_Y_IV_XX) - scalar(delta_Y_`pairwise'`pla'XX)*absdeltaZ_XX
	 }
	 replace Phi_Y_XX = Phi_Y_XX/scalar(EabsdeltaZ_`pairwise'`pla'XX)*scalar(PHt`pairwise'`pla'XX )

	 //ii. Compute phi_D
	 sum innerSumIV_denom_`pairwise'`pla'XX  
	 scalar delta_D_`pairwise'`pla'XX = r(mean)

	 
	 //reg deltaD_XX `vars_pol_XX' if SI_XX==0 
	 reg deltaD_XX `varsIV_pol_XX'     if SI_XX==0 
	 predict mean_pred_D_IV_XX , xb 
	 
	 if ("`exact_match'"==""){	 
	 gen Phi_D_XX = (SI_XX - (PSIPlus1Z1_XX-PSIMinus1Z1_XX)*(1-SIbis_XX)/(PS_IV0Z1_XX))*(deltaD_XX - mean_pred_D_IV_XX) - scalar(delta_D_`pairwise'`pla'XX )*absdeltaZ_XX
	 }
	 else{
	 gen Phi_D_XX = (SI_XX - (ESI_XX_Z1)*(1-SIbis_XX)/(1-ESIbis_XX_Z1))*(deltaD_XX - mean_pred_D_IV_XX) - scalar(delta_D_`pairwise'`pla'XX )*absdeltaZ_XX 	
	 }
	 replace Phi_D_XX = Phi_D_XX/scalar(EabsdeltaZ_`pairwise'`pla'XX)*scalar(PHt`pairwise'`pla'XX )
	 
	 //iii. Now compute Phi_IV
	 gen Phi3_`pairwise'`pla'XX = (Phi_Y_XX - scalar(delta3_`pairwise'`pla'XX)*Phi_D_XX)/scalar(delta_D_`pairwise'`pla'XX )
	 replace Phi3_`pairwise'`pla'XX = 0 if Ht_XX == 0 
	 
	sum Phi3_`pairwise'`pla'XX 
	scalar mean_IF3`pairwise'`pla' = r(mean) //check if the mean is close to zero when I will output the simulations (to be drop in the final version)
	
	if ("`cluster'"!=""){ // Clustering the variance //CLUSTER OPTION
		cap drop Phi3_`pairwise'`pla'_cXX //I create a new variable, because I need the one without clustering in the aggregation
		bysort `cluster': egen Phi3_`pairwise'`pla'_cXX = total(Phi3_`pairwise'`pla'XX)
		bysort `cluster': replace Phi3_`pairwise'`pla'_cXX=. if _n!=1
		replace Phi3_`pairwise'`pla'_cXX = Phi3_`pairwise'`pla'_cXX/scalar(N_bar_c_`pairwise'`pla'XX)
		sum Phi3_`pairwise'`pla'_cXX   //we use the weights of the cluster here
	}
		
	scalar sd_delta3_`pairwise'`pla'XX = r(sd)/sqrt(r(sum_w)) //sqrt(scalar(N_XX)) // Clustering change the N_XX by r(N)

	scalar LB3_`pairwise'`pla'XX = scalar(delta3_`pairwise'`pla'XX) - 1.96*scalar(sd_delta3_`pairwise'`pla'XX)
	scalar UB3_`pairwise'`pla'XX = scalar(delta3_`pairwise'`pla'XX) + 1.96*scalar(sd_delta3_`pairwise'`pla'XX)
}
//End of IV feasible estimation
else{

	scalar delta3_`pairwise'`pla'XX  = . //0
	scalar sd_delta3_`pairwise'`pla'XX = .
	scalar LB3_`pairwise'`pla'XX = .
	scalar UB3_`pairwise'`pla'XX = .
	scalar denom_deltaIV`pairwise'`pla'XX =0
	gen Phi3_`pairwise'`pla'XX = .
	gen innerSumIV_denom_`pairwise'`pla'XX = .
	
	//If we are in gaps cases
		if (scalar(gap_`pairwise'`pla'XX) !=0){
		scalar N_Switchers3_`pairwise'`pla'XX = .
		scalar Nstayers3_`pairwise'`pla'XX = .
		}
		else{
	//If we are in only one type cases
		if (scalar(n_stayersIV_`pla'XX)<2){
		scalar N_Switchers3_`pairwise'`pla'XX = _N - scalar(n_stayersIV_`pla'XX)
		scalar Nstayers3_`pairwise'`pla'XX = scalar(n_stayersIV_`pla'XX)
		}	
		
		if (scalar(n_switchersIV_`pla'XX)==0){
		scalar N_Switchers3_`pairwise'`pla'XX = 0
		scalar Nstayers3_`pairwise'`pla'XX = _N
		}
		}
}
} 
} //End of else from if (_N==0)

if (`bootstrap'==0){ //This is only needed to comupte the aggregated IFs.
sort ID_XX
///////I will merge the datasets here! This is to compute the aggregated influence function
//di as error "Here `pairwise':`data_1XX'"

cap drop absdeltaD_errorXX //This in case was is not requested: just to avoid doing many ifs.
gen absdeltaD_errorXX=.
cap drop S_errorXX 
gen S_errorXX=.
cap drop innerSumIV_denom_errorXX
gen innerSumIV_denom_errorXX = .
//Keep ID_XX, Phi1_t, Phi2_t, S_t, and absdeltaD_t, P_`pairwise'XX
keep ID_XX Phi?_*XX S_*XX absdeltaD_*XX used_in_*_XX innerSumIV_denom_*XX `cluster' weights_XX weights_cXX //Phi* //Ster_XX ESbis_XX_D1 meanS_over_deltaD_XX inner_sumdelta12_XX mean_pred_XX deltaY_XX deltaD_XX outOfBounds_XX
drop absdeltaD_errorXX
drop S_errorXX
drop innerSumIV_denom_errorXX
//di as erro "`pla' `pairwise'"
merge 1:1 ID_XX using "`data_1XX'.dta", gen(merge_`pairwise'`pla'XX)

//Note Doulo: The merge_`pairwise'XX variable will be very usefull if we need afterward to give information about which units are used in each DID when for instance we have unbalanced panel.

if ("`pla'" == "" ) {
tempfile data_1XX
save "`data_1XX'.dta", replace
}
else{
tempfile data_1plaXX
save "`data_1plaXX'.dta", replace	
}
}
if ("`pla'" == "" ) glob data_1XX = "`data_1XX'"
else glob data_1plaXX = "`data_1plaXX'"
//> CORE restore
use "`OG_dataPathcore'.dta", clear

}
//End of quietly

end

/******************************************************************************
								  SUBPROGRAMS
******************************************************************************/
//.
cap program drop parse_select_twfe_suboptions
program parse_select_twfe_suboptions , sclass
    syntax [, percentile normal same_sample full_sample]
	
	// Modif Felix: Allow the user to specify full sample which does nothing (default before was already using the full sample) but in the helpfile we advice the user to specify one of the two within the twfe() option to trigger the option
	// Same logic with the normal for bootstrap (does not change anything with respect to the default from before so if you do not specify percentile or normal it is as if you specify normal)
    
    sreturn local percentile `percentile'
	sreturn local same_sample `same_sample'
end


///////Program dropping a specific list of scalars: To use after
cap program drop scalars_to_drop
program define scalars_to_drop
//efffect
cap drop tag_XX
cap drop to_erase_XX*
gen tag_XX = 1
mata: to_erase_XX = st_dir("global", "numscalar", "*XX")
getmata (to_erase_XX*) = to_erase_XX, force
qui levelsof to_erase_XX, local(scalars) 

foreach l of local scalars {
	if ("`l'"!="N_drop_noextra_XX") scalar drop `l'
}
drop if tag_XX != 1
drop to_erase_XX* tag_XX
//Placebo
gen tag_XX = 1
mata: to_erase_XX = st_dir("global", "numscalar", "*_*plaXX")
getmata (to_erase_XX*) = to_erase_XX, force
qui levelsof to_erase_XX, local(scalars) 

foreach l of local scalars {
	scalar drop `l'
}
drop if tag_XX != 1
drop to_erase_XX* to_erase_XX

//Placebo
gen tag_XX = 1
mata: to_erase_XX = st_dir("global", "numscalar", "*_*plaXX")
getmata (to_erase_XX*) = to_erase_XX, force
qui levelsof to_erase_XX, local(scalars) 

foreach l of local scalars {
	scalar drop `l'
}
drop if tag_XX != 1
drop to_erase_XX* to_erase_XX
end



/*******************************************************************************
SUBPROGRAMS FOR CROSS-VALIDATION
*******************************************************************************/
//1.
cap program drop parse_select_cv_suboptions
program parse_select_cv_suboptions , sclass
    syntax [, ALGOrithm(string) TOLErance(real 0.01) max_k(integer 5) seed(integer 0) kfolds(integer 5)] // Felix: should it be max_k(integer 1) or actually max_k(integer 5) here? -> Modif: using default 5 makes it run and is consistent with what we say in the help file!
    
	// Felix: In the help file now we say that algorithm has to be specified!	
	
    sreturn local algorithm `algorithm'
    sreturn local tolerance `tolerance'
    sreturn local max_k     `max_k'
    sreturn local seed      `seed'
	sreturn local kfolds    `kfolds'
end


capture program drop cross_validation
program define cross_validation, sclass
	version 12.0
	syntax  anything [if] [in] [, ALGOrithm(string) TOLErance(real 0) max_k(integer 5) seed(integer 0) kfolds(integer 5) model(string) cv_covariates(string)] // FIRST_stage  weights(varlist numeric max=1) 
	
	scalar set_chosen_order_linear = 0
	tokenize `anything'
	//di as error "outcome = `1'"
di _newline


if ("`algorithm'"=="kfolds") _dots 0, title(`algorithm'(`kfolds'): Cross validation running) reps(`max_k')

if ("`algorithm'"=="loocv") {
	_dots 0, title(`algorithm': Cross validation running) reps(`max_k')
}

			if ("`model'" == "logit"&"`algorithm'"=="loocv") {
				di as error "logit regression not allowed with loocv."
				exit
			}
//Display 
			if ("`1'" == "deltaYt_XX") local name = "E(Y_t - Y_{t-1}|D_{t-1})"
			if ("`1'" == "S0bist_XX")  local name = "P(S_t = 0|D_{t-1})"
			if ("`1'" == "StPlus_XX")  local name = "P(S+_t = 0|D_{t-1})"
			if ("`1'" == "StMinus_XX") local name = "P(S-_t = 0|D_{t-1})"
	
di as input " "
di as input "{hline 80}"
di as input "Cross-validation on `name': "
di as input "{hline 80}"	
di as input "Models" _skip(18) "CV's Scores"
quietly{
	//>MAIN: Preserve the inputted dataset
	preserve
	
	//dropping observations not included in the if condition
	if "`if'" !=""{
	keep `if'
	}
//mata: mata clear
//Test
cap drop CVs
cap drop Order

gen CVs = .
gen Order = .

}
quietly{
			if ("`algorithm'"=="kfolds"){
								//Shuffle the whole dataset 
				cap drop random_shufller_XX
				set seed `seed'
				gen random_shufller_XX = rnormal()
				sort random_shufller_XX
				
				//Generate the test samples identifier
				cap drop fold_identifier_XX 
				set seed `seed'
				gen fold_identifier_XX = runiformint(1, `kfolds')
				tab fold_identifier_XX
			}
}	
					local counter2 = 0
					
local cv_covariates0
local PolK0 
forvalues k=1/`max_k'{
	
	xtset ID_XX T_XX
	cap drop LD_XX 
	gen LD_XX = L.D_XX
	
cap drop Lag1Dt_`k'XX
gen Lag1Dt_`k'XX = LD_XX^`k'
local PolK`k' "`PolK`=`k'-1'' c.Lag1Dt_`k'XX"
foreach var of varlist T_XX_FE_1-T_XX_FE_`=max_T'{
	cap drop bis`var'_Lag1Dt_`k'XX
	gen  bis`var'_Lag1Dt_`k'XX = `var'*Lag1Dt_`k'XX
}
}

forvalues k=1/`max_k'{
	//Progress Bar
	sleep 1
    _dots `k' 0 

	
*******************This block is to put inside cv subcommand, replacing `max_k' by `k' and k by smtg else********************	
forvalues k2 =1/`k'{
	
	//list of covariates: loocv
	foreach var of varlist bis*_Lag1Dt_`k2'XX{
		local cv_covariates`k'	"`cv_covariates`=`k''' `var'"
		}
}
//di as error "`PolK'"
//local controls_cv "(c.T_XX_FE_*)#(`PolK')"

//list of covariates for order k: kfolds
local controls_cv`k' "(c.T_XX_FE_*)#(`PolK`k'')"

//di as error "`controls_cv`k''"
//di as error "`cv_covariates`k''"
********************************************************************************


			if ("`algorithm'"=="kfolds"){
     // quietly{
				//Run the regressions and compute the mse, cv-scores
				cap drop e_sq_XX
				gen e_sq_XX = . 
				local counter = 0
				forvalues test_sample_id = 1/`kfolds'{
					if ("`model'"==""|"`model'"=="reg") cap reg `anything' `controls_cv`k'' if fold_identifier_XX!=`test_sample_id'
					else {
						 cap `model' `anything' `controls_cv`k'' if fold_identifier_XX!=`test_sample_id', asis
						//di as error "`model' `anything' if fold_identifier_XX!=`test_sample_id'"
					}
					if (_rc==0|_rc==430|_rc==2000){ //convergence not achieved, keep going?
					if (_rc==430) local counter = `counter'+1
					cap drop e_`test_sample_id'_XX
					if ("`model'"==""|"`model'"=="reg") {
						predict e_`test_sample_id'_XX if fold_identifier_XX==`test_sample_id', residuals 
						replace e_sq_XX =  (e_`test_sample_id'_XX)^2 
						}
						else {
							
						predict e_`test_sample_id'_XX if fold_identifier_XX==`test_sample_id' , pr
						//Score = -[ylogpi+(1−y)log(1−pi)]
						replace e_sq_XX = -[`1'*log(e_`test_sample_id'_XX)+(1-`1')*log(1- e_`test_sample_id'_XX)] if fold_identifier_XX==`test_sample_id'
						tab e_`test_sample_id'_XX
					}
					
					}
				}
								
				cap drop mse_XX
				gegen mse_XX = mean(e_sq_XX), by(fold_identifier_XX)
				bysort fold_identifier_XX: replace mse_XX=. if _n!=1
				sum mse_XX
				scalar CV_`k' = r(mean)
				//}
				if (`counter' == `kfolds') {
					local counter2 = `counter2' + 1
					di as error "Model `k' (k=`k')"  _skip(12) scalar(CV_`k') _column(40)  "Convergence not achieved."
					sreturn local set_chosen_order_linear = 1
					scalar set_chosen_order_linear = 1
					continue , break
					//Stop and chose the order for the linear regression.
					
					scalar CV_`k' = .
				} //Not considering this order.
				else{
					di as input "Model `k' (k=`k'):" _skip(12) scalar(CV_`k')
				}
				//save "data_cv.dta", replace
				
				}

			if ("`algorithm'"=="loocv"){
			quietly{
			//qui capture
			cap reg `anything' `controls_cv`k''
			if _rc==0{
			cap drop e_i 
			cap drop id
			predict e_i , residuals
			gen id = _n if !missing(e_i)
			
			mata    A  = st_data(., "`cv_covariates`k''"):!= .
			mata	id = st_data(., "id") :!= .
			mata	A  = select(A, id)
			mata	H  = (J(rows(A), 1,1) - diagonal(A * invsym(A' * A) * A')):^2
			mata	  st_matrix("H", H)
			
			cap drop H1
			svmat H
			cap drop He
			gen He = e_i^2/H1
			sum He
			scalar CV_`k' = r(mean)
			}
			else{
				scalar CV_`k' = .
			}
			}
			
			display as input  "Model `k' (k=`k'):" _skip(12) scalar(CV_`k')
			}
			
			//Selection process according to the tolerance
		//
					//di as error "CV_`k' = " scalar(CV_`k')
					qui replace CVs = scalar(CV_`k') if _n==`k'
					qui replace Order =  `k' if _n==`k'
					
		if (`k'>1){
			local diff = scalar(CV_`k')/scalar(CV_`=`k'-1')-1
			//di as error "Gain: 	`diff'"
			//di as error "if(`diff'>-`tolerance'&`diff'!=.)"
			if(`diff'>-`tolerance'&`diff'!=.){ //If we loose so much information according to the tolerance
			local opt_k = `k'-1
			//dis as red "Optimal K = `opt_k' "
			continue, break
			}
		}
		scalar stoper = `k'
}

if (`counter2'==`max_k'){
	di as error "Cross validation: Convergence not achieved."
}				
//sort Order
//line CVs Order
//Display 
if (scalar(set_chosen_order_linear)!=1){
		if (scalar(stoper)<`max_k') cap . //di as input _skip(0) "The stop criteria is: tolerance =  `tolerance', "
		else {
			//di as red _skip(0) "The stop criteria is: maximum order  = `max_k', "
			
			//local opt_k  = `max_k'
			
			//Take the minimum to avoid case where we do not take the min CV due to the tolerance
quietly{
					sort CVs Order
					sum CVs if _n==1
					local min_opt_k = r(mean)
					sum Order if _n==1
					local opt_k = r(mean)
					//dis as red "True Optimal K = `opt_k' "

	//>MAIN: Restore the inputted dataset
	restore
}	

			}
di as input "The first optimal order found is: k = `opt_k'"

		
			sreturn local chosen_order = `opt_k'
			}
else{
	di as red "The command will then consider the order from the linear regression cv."
}

	sreturn local set_chosen_order_linear = scalar(set_chosen_order_linear) // Modif Felix: initialize sreturn local -> apperently overwritten when using logit

	forvalues k=1/`max_k'{
		cap scalar drop CV_`k'
	}
	scalar drop stoper

end
/*******************************************************************************
SUBPROGRAM POLYNOMIALS GENERATOR
*******************************************************************************/
cap program drop polynomials_generator
program define polynomials_generator, sclass
    syntax [, order(integer 1) pla prefix(string) controls(varlist numeric) other_treatments(varlist numeric)]
    
	// Generating polynomials of the baseline treatement	
		//i for D
			local vars_pol_XX =""  //"`other_treatments'" 
			local vars_pol_controlsXX ="" 
			forvalues pol_level = 1/`order'{
			scalar pol_level_`pla'XX = `pol_level'
			capture drop D1_XX_`pol_level'_XX 
			gen D1_XX_`pol_level'_XX = D1_XX^scalar(pol_level_`pla'XX)
			local vars_pol_XX = "`vars_pol_XX' D1_XX_`pol_level'_XX"
			
			//Controls: Generate the polynomial order of each control
			if ("`controls'"!=""){
				foreach control in `controls'{
					cap drop `control'_`pol_level'_XX
					gen `control'_`pol_level'_XX = `control'^scalar(pol_level_`pla'XX)
					local vars_pol_controlsXX = "`vars_pol_controlsXX' `control'_`pol_level'_XX"
				}
			}
		   	}
		if (`order'>1){ //we only do the interaction when order>2
			//Add the interaction control#D1
			if ("`controls'"!=""&`order'>1){
				foreach control in `controls'{
					local vars_pol_controlsXX = "`vars_pol_controlsXX' c.`control'#c.D1_XX"
				}
			}
			//Add the interaction control#control if order>=2

			if ("`controls'"!=""&`order'>1){
				local i = 0
				foreach control1 in `controls'{
					local i = `i'+1
					local j = 0
					foreach control2 in `controls'{
						local j = `j'+1
						if (`j'>`i') {
							local vars_pol_controlsXX = "`vars_pol_controlsXX' c.`control1'#c.`control2'"
						}
					}
				}
			}	
			}

				//di as error "`vars_pol_controlsXX'"
		//// Other treatments option: More general and inclusive way of doing the interaction.
			if ("`other_treatments'"!=""){
				foreach var in `other_treatments'{
					if ("`interact'"==""){
						local interact  = "c.`var'##c.D1_XX"
					}
					else{
						local interact  = "c.`interact'##c.`var'"
					}
			local vars_pol_XX = "`vars_pol_XX' `interact'"
				}

			}
	
				//All controls
			local vars_pol_XX = "`vars_pol_XX' `vars_pol_controlsXX'"
			
//prefix in {logit_Plus, logit_Minus, logit_bis, reg}

    sreturn local `prefix'_pol_XX `vars_pol_XX'
	//di as error "`s(`prefix'_pol_XX)'"

end

cap program drop  return_label
program define return_label, sclass
syntax [, by_quantile(integer 1) by_fd(integer 1) q(integer 1) as(integer 0) was(integer 0) iwas(integer 0)  display_message]
if (`by_quantile'>1){ //I will use this to label the graph
	local lb = scalar(r`=`q'-1'_rr)
	local ub = scalar(r`q'_rr)
	local label_q_`q' = "[`lb'; `ub'["
	 if (`by_fd'>1){
		if (`as'==1| `was'==1){	
		 if ("`display_message'"!="")  di as input _skip(25) "DeltaD in `label_q_`q''"
		 sreturn local label_quantile`q' = "DeltaD in `label_q_`q''"
		}
		if (`iwas'==1) {
			if ("`display_message'"!="") di as input _skip(25) "DeltaZ in `label_q_`q''"
			sreturn local label_quantile`q' =  "DeltaZ in `label_q_`q''"
		}
	 }
	 else{
		if (`as'==1| `was'==1){
			if ("`display_message'"!="")  di as input _skip(25) "Baseline D in `label_q_`q''"
			sreturn local label_quantile`q' =  "Baseline D in `label_q_`q''"
		}
		if (`iwas'==1)  {
			if ("`display_message'"!="")  di as input _skip(25) "Baseline Z in `label_q_`q''"	 	
			sreturn local label_quantile`q' =  "Baseline Z in `label_q_`q''"	 	
		}
	 }
	
	//label define by_quantile_label  `q' "`label_q_`q''", add
	//label list
	//di as error "`s(label_quantile`q')'"
}
end

