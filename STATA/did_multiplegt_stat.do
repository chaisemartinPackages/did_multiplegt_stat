*This program estimates the three estimators (aoss, waoss, iv-aoss) developped in 
**de Chaisemartin, Clément and d'Haultfoeuille, Xavier and Pasquier, Félix and Vazquez‐Bare, Gonzalo,
** Difference-in-Differences Estimators for Treatments Continuously Distributed at Every Period (January 18, 2022). 
**Available at SSRN: https://ssrn.com/abstract=4011782 or http://dx.doi.org/10.2139/ssrn.4011782

//Doulo: on Jan, 4: this versions compute the aoss - waoss - iwaoss estimators, and theirs variances with options (polynom, switchers)

//1. The simulations made with this version look good: ~96% of coverage, with 100 simulations (test_simu_madeupData1.do)

//2. At this date, Janv 4, the command does not consider the logit, and the doubly_robust option,  appraoches yet.

//3. The number of periods is >=2

//4. Missing values problem is solved - unbalanced data

//Main options: estimator(aoss|waoss|iwaoss), estimation_method(ra|ps|dr), order(), switchers(up|down), noextrapolation, placebo, disaggregate


*The aggregation is done, and is well working: as test I tried to also to test whether I have the same variance if T=2 for both the aggregated and the simple case. 
*noextrapolation coded
		
//To be added checked: Consider the difference combination (aoss, waoss, iwaoss) in if conditions //solved

//Janv, 15: the new versions of the IFs are implemented

//panel data with gaps or one type cases are also coded

//Janv, 17 I started coding the ps-based approach for the waoss, starting from the ra-based approach version
//I added the dr-based approach

//Janv, 29: Some adjustements to wrap up: 
	/* 1. For the aoss, \hat{E}(S|D_1) in the influence function is no longer estimated by a linear regression but by a logit 
	/All the methods (ra, ps, dr) are computed for aoss, waoss, and iwaoss as well. Need to do the aggreagation for iwaoss (done!).
	*/
//Janv 30, the aggregated and disaggregated placebos versions are computed.

///FEBRUARY, 1: This version computes all the options provided in the syntax, except only the aggreagation of IWAOSS.
//FEB, 8: The command is more or less done, I made several checks (coverage rate, influence functions, points estimate, sanity-checks etc.) and we are find so far.

//Feb, 19: Option cluster is computed. 

//Feb 21: The weight option: The idea is (i) to exploit the command {sum varlist [iw = weight_XX]}, (ii) put the option weight in all the regressions (logit and linear), (iii) instead of usingg r(N) use r(sum_w) etc.

//Feb, 23: Shut down the option weight and cluster: local weight = "" and local cluster = ""

capture program drop did_multiplegt_stat
program did_multiplegt_stat, eclass sortpreserve
	version 12.0
	syntax varlist(min=4 max=5 numeric) [if] [in] [, estimator(string) estimation_method(string) ORder(integer 0) NOEXTRApolation placebo switchers(string) DISAGgregate aoss_vs_waoss exact_match ] // cluster(varlist max=1) weight(varlist numeric max=1) 
	
	//>MAIN: Preserve the inputted dataset
	preserve
quietly{

//Shut down the option weight and cluster: 
local weight = "" 
local cluster = ""

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
cap drop weight_XX
cap drop weight_cXX
cap drop S_XX
cap drop D1_XX
cap drop deltaD_XX
cap drop deltaY_XX
cap drop to_drop_XX
cap drop tsfilled_XX
cap drop T_OG_XX


///// Collapse and weight
	
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
	
	replace weight_XX=0 if `1'==.
	
	if "`1'"!="`4'"{
				//Averging the outcome and treatment
		collapse (mean) `1' `4' (count) weight_XX [pw=weight_XX], by(`2' `3')
	}
	
	if "`1'"=="`4'"{			
		collapse (mean) `1' (count) weight_XX [pw=weight_XX], by(`2' `3')
	}	
}
*/

//gen main varlist
gen Y_XX = `1'
gen ID_XX =  `2'
gen T_OG_XX = `3'
gen D_XX = `4' 

local OG_nameID_XX = "`2'"
//2. IV method:
local IV_feed_XX = "no"

if ("`5'"!=""&"`5'"!=","){
local IV_feed_XX = "yes"
local IV_var_XX  `5' 
}

}

 xtset `2'  `3' //make the quietly skip that to show the characteristics of the panel: balanced/unbalanced/w|o gaps etc.
 
quietly{
/*******************************************************************************
//Check all the estimators that are requested - to customize the display
*******************************************************************************/

	local 1 ""
	local 2 ""
	local 3 ""
	
if("`estimator'" ==""){
	if ("`IV_feed_XX'" == "no"){
	local 1 "aoss"
	local 2 "waoss"
	//local 3 "iwaoss" //Now the default (when estimator is not specified) is to compute aoss and waoss only if Z is not given
	local aoss_XX = 1
	local waoss_XX = 1
	local iwaoss_XX= 0
	}
	else{
	//local 1 "aoss"
	//local 2 "waoss"
	local 3 "iv-waoss" //Now the default (when estimator is not specified) is to computes iwaoss only if Z is specify
	local aoss_XX = 0
	local waoss_XX = 0
	local iwaoss_XX= 1
	}
}
else{
	//Count the number of estimators requested
	//scalar nb_estimatorts_XX = `:word count `estimator''
		tokenize `estimator'
		local aoss_XX = inlist("aoss", "`1'", "`2'", "`3'")
		local waoss_XX = inlist("waoss", "`1'", "`2'", "`3'")
		local iwaoss_XX= inlist("iv-waoss", "`1'", "`2'", "`3'")
		
		if ("`aoss_XX'"=="1"){
			local 1 = "aoss" 
		}
		if ("`waoss_XX'"=="1"){
			local 2 = "waoss" 
		}
		if ("`iwaoss_XX'"=="1"){
			local 3 = "iv-waoss" 
		}

}

local total_estimator = `aoss_XX' + `waoss_XX' + `iwaoss_XX'
/********************************************************************************
			ERRORS MESSAGES FOR :
********************************************************************************/
//0. Estimators
if (`total_estimator'==0){
		di as error "Error in the option estimator."
		di as error "The arguments allowed are: aoss, waoss, or iv-waoss."
		exit
}
//1. Estimation method:

if ("`estimation_method'" == "ps"|"`estimation_method'" == "dr"){
	if ("`waoss_XX'"=="0"&"`iwaoss_XX'"=="0"){
		di as error "The propensity/doubly-robust -based approach is only available for the waoss and the iv-waoss."
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
if ("`IV_feed_XX'"=="no"&("`iwaoss_XX'" == "1")){
	di as error "To compute the iwaoss you must specify the IV variable."
	exit
}


//3. Combination of estimators //Shut this consteraint down
/*
if ("`iwaoss_XX'" == "1"&("`waoss_XX'" == "1"|"`aoss_XX'" == "1")){
	di as error "The estimation of AOSS or WAOSS cannot be combined with the estimation of IV-WAOSS (see helpfile)."
	exit
}
*/

//4. The test of equality between aoss and waoss
local a_vs_w = `aoss_XX' + `waoss_XX'
if ("`aoss_vs_waoss'"!=""&`a_vs_w'!=2){
	di as error "To test the equility between AOSS and WAOSS you must specify aoss and waoss in the estimator option."
	exit
}

//5.
if (`total_estimator'==0){
		di as error "Error in the option estimator."
		di as error "The arguments allowed are: aoss, waoss, or iv-waoss."
		exit
}

//6. exact_match and noextrapolation
if ("`exact_match'"!=""&"`noextrapolation'"!=""){
		di as error "As the exact_match option is specified,"
		di as error "the noextrapolation option is ignored."
	local noextrapolation = ""
}

//7. exact_match and [order, estimation_method, order]
if ("`exact_match'"!=""){
	if ("`estimation_method'"!=""){
		di as error "As the exact_match option is specified,"
		di as error "the estimation_method option is ignored."
	}
	
	if (`order'!=0){ //The values are then set within the pairwise program.  To se where search local order = r(r)
		di as error "As the exact_match option is specified,"
		di as error "the order option is ignored."
	}
local estimation_method = "ra"	//set the default if exact_match>
	
}
else{
	if (`order'==0){
		local order =1 //Set the default to 1 if the option is not specified and without exact_match
}
	if ((`waoss_XX'==1|`iwaoss_XX'==1)&"`estimation_method'"==""){
		local estimation_method = "dr" //default option
		
	}
}

//8. Cluster non-nested  //CLUSTER OPTION
if ("`cluster'"!=""&"`cluster'"!="`OG_nameID_XX'"){
	bysort ID_XX: gegen cluster_sd_XX = sd(`cluster')
	sum cluster_sd_XX
	scalar max_cluster_sd_XX = `r(max)'
	if (scalar(max_cluster_sd_XX) >0){
	di as error ""
	di as error "The ID variable should be nested within the clustering variable."
	exit
	}
}	
********************************************************************************

//Handle missing values i)
gen to_drop_XX = (Y_XX==.|T_OG_XX==.|D_XX==.|ID_XX==.)
//Handle missing values ii)
if ("`iwaoss_XX'" == "1"){
replace to_drop_XX = (to_drop_XX==.|`IV_var_XX'==.)
}
//Handle missing values iii)END
drop if to_drop_XX


//****************************If there is gap
gen tsfilled_XX = 0

xtset ID_XX T_OG_XX
tsfill, full
replace tsfilled_XX = 1 if tsfilled_XX==.
sum tsfilled_XX

********************************************//

gegen T_XX =  group(T_OG_XX)
//save "Tsfilled.dta"

// Creating the weight variable. //WEIGHT OPTION //CLUSTER OPTION 

if("`weight'"==""){
gen weight_XX  = 1
gen weight_cXX = 1 
}
else{
gen weight_XX = `weight'
if ("`cluster'"!=""){
	bysort `cluster' T_XX : gegen weight_cXX = total(weight_XX)
}
}

replace weight_XX=0 if weight_XX==.
replace weight_cXX=0 if weight_cXX==.

*******************************************************

//Keep a dataset with all the IDs: this is very helpful for the computation of tha aggregated influence function (Doulo: Think about the missing values -unbalanced panel- to remember why I prefered this approach over the matrix approach.)

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
	

**# Bookmark #1 Generate the time pairwise dummies
sum T_XX
scalar max_T = r(max)

if ("`placebo'"!=""&scalar(max_T)==2){
	di as error "Placebo cannot be estimated with only two periods."
	di as error "The command will then ignore the option placebo."
	local placebo ""
	
}
if (scalar(max_T)<2){
	di as error "Your time variable takes less than two values."
	exit
}

**# Bookmark #2 AGGREGATION TO OBTAIN delta_1, delta_2, and delta_3 and their variances

//Some initialisations for the aggregation
scalar PS_sum_XX           = 0
scalar delta1_1XX          = 0

scalar EabsdeltaD_sum_XX   = 0
scalar delta2_1XX          = 0

scalar denom_deltaIV_sum_XX   = 0
scalar delta3_1XX              = 0 

scalar N_Switchers2_1XX    = 0
scalar Nstayers2_1XX       = 0

scalar N_Switchers1_1XX    = 0
scalar Nstayers1_1XX       = 0

scalar N_Switchers3_1XX    = 0
scalar Nstayers3_1XX       = 0

scalar N_drop_noextra_XX = 0 
scalar N_drop_c_noextra_XX = 0 
//Placebos' versions
if("`placebo'"!=""){
scalar PS_sum_plaXX           = 0
scalar delta1_1plaXX          = 0

scalar EabsdeltaD_sum_plaXX   = 0
scalar delta2_1plaXX          = 0

scalar denom_deltaIV_sum_plaXX   = 0
scalar delta3_1plaXX              = 0 

scalar N_Switchers2_1plaXX    = 0
scalar Nstayers2_1plaXX       = 0

scalar N_Switchers1_1plaXX    = 0
scalar Nstayers1_1plaXX       = 0

scalar N_Switchers3_1plaXX    = 0
scalar Nstayers3_1plaXX       = 0

scalar N_drop_noextra_plaXX = 0 
scalar N_drop_c_noextra_plaXX = 0 
}

**# Bookmark #2bis Call the program for each time pairwise dummy

//Call the command

forvalues p = 2/`=max_T'{
	//i) Calling the command for each pair of time periods
	did_multiplegt_stat_pairwise Y_XX ID_XX T_XX D_XX `IV_var_XX' `if' `in' , estimator(`estimator') or(`order') `noextrapolation' weight(weight_XX) switchers(`switchers') pairwise(`p') data_1XX($data_1XX) aoss(`aoss_XX') waoss(`waoss_XX') iwaoss(`iwaoss_XX') estimation_method(`estimation_method') `exact_match' cluster(`cluster')

	//i) Aggregation as the loop goes
	
	//aoss
	if (`aoss_XX' == 1){
		if (scalar(delta1_`p'XX)!=.){
		scalar delta1_1XX = scalar(delta1_1XX) + scalar(P_`p'XX)*scalar(delta1_`p'XX)
		
		//Numbers of switchers and stayers: Note that if the estimation is not feasible we do not add the number of switchers/stayers in the total number
		scalar N_Switchers1_1XX = scalar(N_Switchers1_1XX) + N_Switchers1_`p'XX
		scalar Nstayers1_1XX    = scalar(Nstayers1_1XX) + Nstayers1_`p'XX
		}
	}
	
	//waoss
	if (`waoss_XX' == 1){
		if (scalar(delta2_`p'XX)!=.){
		scalar delta2_1XX = scalar(delta2_1XX) + scalar(EabsdeltaD_`p'XX)*scalar(delta2_`p'XX)
		
		//Numbers of switchers and stayers: Note that if the estimation is not feasible we do NOT add the number of switchers/stayers in the total number
		scalar N_Switchers2_1XX = scalar(N_Switchers2_1XX) + N_Switchers2_`p'XX
		scalar Nstayers2_1XX    = scalar(Nstayers2_1XX) + Nstayers2_`p'XX
		}
	}
	
	//iwaoss
	if (`iwaoss_XX' == 1){
		if (scalar(delta3_`p'XX)!=.){
		scalar delta3_1XX = scalar(delta3_1XX) + scalar(denom_deltaIV`p'XX)*scalar(delta3_`p'XX)
		
		//Numbers of switchers and stayers: Note that if the estimation is not feasible we do NOT add the number of switchers/stayers in the total number
		scalar N_Switchers3_1XX = scalar(N_Switchers3_1XX) + N_Switchers3_`p'XX
		scalar Nstayers3_1XX    = scalar(Nstayers3_1XX) + Nstayers3_`p'XX
		}
	}
	
} //END OF THE LOOP effects

if ("`placebo'"!=""){
	forvalues p = 3/`=max_T'{
	//i) Calling the command for each pair of time periods
	did_multiplegt_stat_pairwise Y_XX ID_XX T_XX D_XX `IV_var_XX' `if' `in' , estimator(`estimator') or(`order') `noextrapolation' weight(weight_XX) switchers(`switchers') pairwise(`p') data_1XX($data_1XX) aoss(`aoss_XX') waoss(`waoss_XX') iwaoss(`iwaoss_XX') estimation_method(`estimation_method') `placebo' `exact_match' cluster(`cluster')

	//i) Aggregation as the loop goes
	
	//aoss
	if (`aoss_XX' == 1){
		if (scalar(delta1_`p'plaXX)!=.){
		scalar delta1_1plaXX = scalar(delta1_1plaXX) + scalar(P_`p'plaXX)*scalar(delta1_`p'plaXX)
		
		//Numbers of switchers and stayers: Note that if the estimation is not feasible we do add the number of switchers/stayers in the total number
		scalar N_Switchers1_1plaXX = scalar(N_Switchers1_1plaXX) + scalar(N_Switchers1_`p'plaXX)
		scalar Nstayers1_1plaXX    = scalar(Nstayers1_1plaXX) + scalar(Nstayers1_`p'plaXX)
		}
	}
	
	//waoss
	if (`waoss_XX' == 1){
		if (scalar(delta2_`p'plaXX)!=.){
		scalar delta2_1plaXX = scalar(delta2_1plaXX) + scalar(EabsdeltaD_`p'plaXX)*scalar(delta2_`p'plaXX)
		
		//Numbers of switchers and stayers: Note that if the estimation is not feasible we do add the number of switchers/stayers in the total number
		scalar N_Switchers2_1plaXX = scalar(N_Switchers2_1plaXX) + scalar(N_Switchers2_`p'plaXX)
		scalar Nstayers2_1plaXX    = scalar(Nstayers2_1plaXX) + scalar(Nstayers2_`p'plaXX)
		}
	}
	
	//iwaoss
	if (`iwaoss_XX' == 1){
		if (scalar(delta3_`p'plaXX)!=.){
		scalar delta3_1plaXX = scalar(delta3_1plaXX) + scalar(denom_deltaIV`p'plaXX)*scalar(delta3_`p'plaXX)
		
		//Numbers of switchers and stayers: Note that if the estimation is not feasible we do NOT add the number of switchers/stayers in the total number
		scalar N_Switchers3_1plaXX = scalar(N_Switchers3_1plaXX) + scalar(N_Switchers3_`p'plaXX)
		scalar Nstayers3_1plaXX    = scalar(Nstayers3_1plaXX) + scalar(Nstayers3_`p'plaXX)
		}
	}
	
} //END OF THE LOOP PLACEBO

}
    //iii) Compute the aggregated estimators (Effects and Placebos)
	
	//aoss
	if (`aoss_XX' == 1){
		scalar delta1_1XX = scalar(delta1_1XX)/scalar(PS_sum_XX)
		
		if ("`placebo'"!="") scalar delta1_1plaXX = scalar(delta1_1plaXX)/scalar(PS_sum_plaXX)
	}
	
	//waoss
	if (`waoss_XX' == 1){
		scalar delta2_1XX = scalar(delta2_1XX)/scalar(EabsdeltaD_sum_XX)
		
		if ("`placebo'"!="") scalar delta2_1plaXX = scalar(delta2_1plaXX)/scalar(EabsdeltaD_sum_plaXX)
	}
	
	//iwaoss
	if (`iwaoss_XX' == 1){
		scalar delta3_1XX = scalar(delta3_1XX)/scalar(denom_deltaIV_sum_XX)
		
		if ("`placebo'"!="") scalar delta3_1plaXX = scalar(delta3_1plaXX)/scalar(denom_deltaIV_sum_plaXX)
	}
	
	
	
	//iv) Compute the influence functions
	
	//preserve 2
	tempfile OG_dataPath2
	save "`OG_dataPath2'.dta", replace
	
	use "$data_1XX.dta", clear
	
	//save TestOfPhis.dta, replace //store the datasets with all the IFs just for the sake of checking.
	macro drop data_1XX //Drop the glob
	
	//Effects
	local Phi1_ts_XX 
	local Phi2_ts_XX 
	local Phi3_ts_XX
	
	//Placebos
	local Phi1_ts_plaXX 
	local Phi2_ts_plaXX 
	local Phi3_ts_plaXX 
	
	forvalues p = 2/`=max_T'{
		
	//aoss: Phi1^{T>2}
	if (`aoss_XX' == 1){
		//Effects
		replace Phi1_`p'XX = [scalar(P_`p'XX)*Phi1_`p'XX + (scalar(delta1_`p'XX) - scalar(delta1_1XX))*(S_`p'XX - scalar(P_`p'XX))]/scalar(PS_sum_XX)
		local Phi1_ts_XX `Phi1_ts_XX' Phi1_`p'XX
		//Placebos
		if ("`placebo'"!=""&`p'>2){
			replace Phi1_`p'plaXX = [scalar(P_`p'plaXX)*Phi1_`p'plaXX + (scalar(delta1_`p'plaXX) - scalar(delta1_1plaXX))*(S_`p'plaXX - scalar(P_`p'plaXX))]/scalar(PS_sum_plaXX)
			local Phi1_ts_plaXX `Phi1_ts_plaXX' Phi1_`p'plaXX
		}
	}
	
	//waoss: Phi2^{T>2}
	if (`waoss_XX' == 1){
		
		//Effects
		replace Phi2_`p'XX = [scalar(EabsdeltaD_`p'XX)*Phi2_`p'XX + (scalar(delta2_`p'XX) - scalar(delta2_1XX))*(absdeltaD_`p'XX - scalar(EabsdeltaD_`p'XX))]/scalar(EabsdeltaD_sum_XX)
		
		////tEST
		sum  Phi2_`p'XX
		scalar mean_IF2bis_`p' = r(mean)
		////
		local Phi2_ts_XX `Phi2_ts_XX' Phi2_`p'XX
		
		//Placebos
		if ("`placebo'"!=""&`p'>2){
			replace Phi2_`p'plaXX = [scalar(EabsdeltaD_`p'plaXX)*Phi2_`p'plaXX + (scalar(delta2_`p'plaXX) - scalar(delta2_1plaXX))*(absdeltaD_`p'plaXX - scalar(EabsdeltaD_`p'plaXX))]/scalar(EabsdeltaD_sum_plaXX)
			local Phi2_ts_plaXX `Phi2_ts_plaXX' Phi2_`p'plaXX
		}
	}
		
	//iwaoss: Phi3^{T>2}
	if (`iwaoss_XX' == 1){
		
		//Effects
		replace Phi3_`p'XX = [scalar(denom_deltaIV`p'XX)*Phi3_`p'XX + (scalar(delta3_`p'XX) - scalar(delta3_1XX))*(innerSumIV_denom_`p'XX - scalar(denom_deltaIV`p'XX))]/scalar(denom_deltaIV_sum_XX)
		local Phi3_ts_XX `Phi3_ts_XX' Phi3_`p'XX
		//Placebos
		
		if ("`placebo'"!=""&`p'>2){
			replace Phi3_`p'plaXX = [scalar(denom_deltaIV`p'plaXX)*Phi3_`p'plaXX + (scalar(delta3_`p'plaXX) - scalar(delta3_1plaXX))*(innerSumIV_denom_`p'plaXX - scalar(denom_deltaIV`p'plaXX))]/scalar(denom_deltaIV_sum_plaXX)
			local Phi3_ts_plaXX `Phi3_ts_plaXX' Phi3_`p'plaXX
		}
	}
	} //END OF THE LOOP
	
	//aoss
	if (`aoss_XX' == 1){
		
		//Effect
		cap drop Phi1_XX
		cap drop not_to_use1_XX
		//di as error "`Phi1_ts_XX'"
		gegen Phi1_XX = rowtotal(`Phi1_ts_XX')
		gegen not_to_use1_XX = rownonmiss(`Phi1_ts_XX') //Count the number of nonmiss, then if not_to_use_XX==0, we set Phi1_XX==.
		replace Phi1_XX=. if not_to_use1_XX==0
		
		sum Phi1_XX [iweight=weight_XX] 
		scalar mean_IF1 = r(mean) //for test 
		
		if ("`cluster'"!=""){ // Clustering the variance //CLUSTER OPTION
		cap drop Phi1_cXX
		bysort `cluster': gegen Phi1_cXX = total(Phi1_XX)
		bysort `cluster': replace Phi1_cXX=. if _n!=1
			if ("`weight'"!=""){
			sum Phi1_cXX [iweight=weight_cXX]  // use the weight of the cluster, which is the total of the weights of groups in that cluster.
			}
			else{
			sum Phi1_cXX [iweight=weight_XX] 
			}

		}
		
		scalar sd_delta1_1XX = r(sd)/sqrt(r(sum_w)) //Doulo: In case of unbalanced panel, some units do not contribute to the influence function at some dates, what about the N, in the asymptotic normal distribution?
		
		scalar LB1_1XX = delta1_1XX - 1.96*sd_delta1_1XX
		scalar UB1_1XX = delta1_1XX + 1.96*sd_delta1_1XX
		
		//Placebo
		if ("`placebo'"!=""){		
			cap drop Phi1_plaXX
			cap drop not_to_use1_plaXX
			gegen Phi1_plaXX = rowtotal(`Phi1_ts_plaXX')
			gegen not_to_use1_plaXX = rownonmiss(`Phi1_ts_plaXX') 
			replace Phi1_plaXX=. if not_to_use1_plaXX==0
			
			sum Phi1_plaXX [iweight=weight_XX] 
			scalar mean_IF1pla = r(mean) //for test 
			
			if ("`cluster'"!=""){ // Clustering the variance //CLUSTER OPTION
			cap drop Phi1_cXX
			bysort `cluster': gegen Phi1_placXX = total(Phi1_plaXX)
			bysort `cluster': replace Phi1_placXX=. if _n!=1

				if ("`weight'"!=""){
				sum Phi1_placXX [iweight=weight_cXX] // use the weight of the cluster, which is the total of the weights of groups in that cluster.
				}
				else{
				sum Phi1_placXX [iweight=weight_XX] 
				}
			}
		
			scalar sd_delta1_1plaXX = r(sd)/sqrt(r(sum_w)) 
			
			scalar LB1_1plaXX = delta1_1plaXX - 1.96*sd_delta1_1plaXX
			scalar UB1_1plaXX = delta1_1plaXX + 1.96*sd_delta1_1plaXX
		}
	}
	
	//waoss
	if (`waoss_XX' == 1){
		
		//Effect
		cap drop Phi2_XX
		cap drop not_to_use2_XX
		gegen Phi2_XX = rowtotal(`Phi2_ts_XX')
        gegen not_to_use2_XX = rownonmiss(`Phi2_ts_XX') //Count the number of nonmiss, then if not_to_use_XX==0, we set Phi2_XX==.
		replace Phi2_XX=. if not_to_use2_XX==0
		
		sum Phi2_XX [iweight=weight_XX] 
		scalar mean_IF2 = r(mean) //for test
		
		if ("`cluster'"!=""){ // Clustering the variance //CLUSTER OPTION
		cap drop Phi2_cXX
		bysort `cluster': gegen Phi2_cXX = total(Phi2_XX)
		bysort `cluster': replace Phi2_cXX=. if _n!=1

				if ("`weight'"!=""){
				sum Phi2_cXX [iweight=weight_cXX] // use the weight of the cluster, which is the total of the weights of groups in that cluster.
				}
				else{
				sum Phi2_cXX [iweight=weight_XX] 
				}
		}
		
		scalar sd_delta2_1XX = r(sd)/sqrt(r(sum_w))
		
		scalar LB2_1XX = delta2_1XX - 1.96*sd_delta2_1XX
		scalar UB2_1XX = delta2_1XX + 1.96*sd_delta2_1XX
		
		//Placebo
		if ("`placebo'"!=""){		
			cap drop Phi2_plaXX
			cap drop not_to_use2_plaXX
			gegen Phi2_plaXX = rowtotal(`Phi2_ts_plaXX')
			gegen not_to_use2_plaXX = rownonmiss(`Phi2_ts_plaXX') 
			replace Phi2_plaXX=. if not_to_use2_plaXX==0
			
			sum Phi2_plaXX [iweight=weight_XX] 
			scalar mean_IF2pla = r(mean) //for test 
			
		if ("`cluster'"!=""){ // Clustering the variance //CLUSTER OPTION
		cap drop Phi2_placXX
		bysort `cluster': gegen Phi2_placXX = total(Phi2_plaXX)
		bysort `cluster': replace Phi2_placXX=. if _n!=1
				if ("`weight'"!=""){
				sum Phi2_placXX [iweight=weight_cXX] // use the weight of the cluster, which is the total of the weights of groups in that cluster.
				}
				else{
				sum Phi2_placXX [iweight=weight_XX] 
				}
		}
		
			scalar sd_delta2_1plaXX = r(sd)/sqrt(r(sum_w)) 
			
			scalar LB2_1plaXX = delta2_1plaXX - 1.96*sd_delta2_1plaXX
			scalar UB2_1plaXX = delta2_1plaXX + 1.96*sd_delta2_1plaXX
		}
	}

	//iwaoss
	if (`iwaoss_XX' == 1){
		
		//Effect
		cap drop Phi3_XX
		cap drop not_to_use3_XX
		gegen Phi3_XX = rowtotal(`Phi3_ts_XX')
        gegen not_to_use3_XX = rownonmiss(`Phi3_ts_XX') //Count the number of nonmiss, then if not_to_use_XX==0, we set Phi2_XX==.
		replace Phi3_XX=. if not_to_use3_XX==0
		
		sum Phi3_XX [iweight=weight_XX] 
		scalar mean_IF3 = r(mean) //for test
		
		if ("`cluster'"!=""){ // Clustering the variance //CLUSTER OPTION
		cap drop Phi3_cXX
		bysort `cluster': gegen Phi3_cXX = total(Phi3_XX)
		bysort `cluster': replace Phi3_cXX=. if _n!=1
		
				if ("`weight'"!=""){
				sum Phi3_cXX [iweight=weight_cXX] // use the weight of the cluster, which is the total of the weights of groups in that cluster.
				}
				else{
				sum Phi3_cXX [iweight=weight_XX] 
				}
		}
		
		scalar sd_delta3_1XX = r(sd)/sqrt(r(sum_w))
		
		scalar LB3_1XX = delta3_1XX - 1.96*sd_delta3_1XX
		scalar UB3_1XX = delta3_1XX + 1.96*sd_delta3_1XX
		
		//Placebo
		if ("`placebo'"!=""){		
			cap drop Phi3_plaXX
			cap drop not_to_use3_plaXX
			gegen Phi3_plaXX = rowtotal(`Phi3_ts_plaXX')
			gegen not_to_use3_plaXX = rownonmiss(`Phi3_ts_plaXX') 
			replace Phi3_plaXX=. if not_to_use3_plaXX==0
			
			sum Phi3_plaXX [iweight=weight_XX] 
			scalar mean_IF3pla = r(mean) //for test 
			
		if ("`cluster'"!=""){ // Clustering the variance //CLUSTER OPTION
		cap drop Phi3_placXX
		bysort `cluster': egen Phi3_placXX = total(Phi3_plaXX)
		bysort `cluster': replace Phi3_placXX=. if _n!=1
		
				if ("`weight'"!=""){
				sum Phi3_placXX [iweight=weight_cXX] // use the weight of the cluster, which is the total of the weights of groups in that cluster.
				}
				else{
				sum Phi3_placXX [iweight=weight_XX] 
				}
		}
			scalar sd_delta3_1plaXX = r(sd)/sqrt(r(sum_w)) 
			
			scalar LB3_1plaXX = delta3_1plaXX - 1.96*sd_delta3_1plaXX
			scalar UB3_1plaXX = delta3_1plaXX + 1.96*sd_delta3_1plaXX
		}
	}
	//save TestOfPhis.dta, replace //For tests
	
/******************************************************************************
TESTING THE DIFFERENCE BETWEEN AOSS AND WAOSS IF REQUESTED

Note: Let UPhi1 and UPhi2 be the non-demeaned versions of Phi1 and Phi2
Testing H_0: aoss = waoss is equivalent to testing H0: meanUPhi1 = meanUPhi2
H0: meanUPhi1 = meanUPhi2 <=> H0: mean(UPhi1 - UPhi2) = 0.
Under H_0, mean(UPhi1 - UPhi2) = 0 <=> mean(Phi1 - Phi2) = 0.
Then we can just test whether the variable  diff_Phi1_2_XX = Phi1_XX - Phi2_XX
is mean-zero or not. This is also equivalent to run reg diff_Phi1_2_XX.
*******************************************************************************/
if ("`aoss_vs_waoss'"!=""&`a_vs_w'==2){
	scalar diff_delta1_2_XX = scalar(delta1_1XX) - scalar(delta2_1XX)
	cap drop diff_Phi1_2_XX 
	gen  diff_Phi1_2_XX = Phi1_XX - Phi2_XX
	sum diff_Phi1_2_XX
    scalar sd_diff_Phi1_2_XX = r(sd)
	scalar tstat_XX = scalar(diff_delta1_2_XX)*sqrt(r(N))/sd_diff_Phi1_2_XX
	scalar pval_XX = 2*(1-normal(abs(tstat_XX)))
	
	matrix aoss_vs_waoss_mat = J(1,6,.)
	matrix aoss_vs_waoss_mat[1,1] = scalar(diff_delta1_2_XX)
	matrix aoss_vs_waoss_mat[1,2] = scalar(sd_diff_Phi1_2_XX)
	matrix aoss_vs_waoss_mat[1,3] = scalar(diff_delta1_2_XX) - 1.96*scalar(sd_diff_Phi1_2_XX)/sqrt(r(N))
	matrix aoss_vs_waoss_mat[1,4] = scalar(diff_delta1_2_XX) + 1.96*scalar(sd_diff_Phi1_2_XX)/sqrt(r(N))
	matrix aoss_vs_waoss_mat[1,5] = scalar(pval_XX)
	matrix aoss_vs_waoss_mat[1,6] = scalar(tstat_XX)
	
	matrix colnames aoss_vs_waoss_mat = "Diff." "SE" "LB CI" "UB CI" "pval." "t"
    matrix rownames aoss_vs_waoss_mat = "AOSS-WAOSS"
	local myN = r(N)
}

	// restore 2
	use "`OG_dataPath2'.dta", clear

	
}


**# Bookmark #4 Output display

scalar nb_rows_XX = 3*(scalar(max_T))
local rownames_XX 
matrix res_mat_XX = J(`=nb_rows_XX',6,.) 

if ("`placebo'"!=""){
	scalar nb_rows_plaXX = 3*(scalar(max_T))
	local rownames_plaXX 
	matrix res_mat_plaXX = J(`=nb_rows_XX',6,.) 
	
	forvalues i=1/3{ //Just a way to show that it is not possible to compute the placebo for (1, 2)
		scalar Nstayers`i'_2plaXX = .
		scalar N_Switchers`i'_2plaXX = .
		scalar delta`i'_2plaXX = .
		scalar UB`i'_2plaXX = .
		scalar LB`i'_2plaXX = .
		scalar sd_delta`i'_2plaXX =.
		}
}

***********************************Fill up the Effects' matrix: START*********************************
*********************************************************************************************

/************************************EFFECTS*****************************************
*************************************************************************************/
//i. Values
forvalues P = 1/3{
forvalues p = 1/`=max_T'{
	scalar P_XX = `=`P''
	scalar index_XX = (scalar(P_XX)-1)*(scalar(max_T))
	if (`aoss_XX' == 1&`P'==1){
		
			**********************************************************	
	matrix res_mat_XX[`=index_XX' + `p',1] = scalar(delta`P'_`p'XX)
	matrix res_mat_XX[`=index_XX' + `p',2] = sd_delta`P'_`p'XX
	matrix res_mat_XX[`=index_XX' + `p',3] = LB`P'_`p'XX
	matrix res_mat_XX[`=index_XX' + `p',4] = UB`P'_`p'XX
	matrix res_mat_XX[`=index_XX' + `p',5] = N_Switchers`P'_`p'XX
	matrix res_mat_XX[`=index_XX' + `p',6] = Nstayers`P'_`p'XX
	
	if ("`placebo'"!=""){
		
			**********************************************************	
			matrix res_mat_plaXX[`=index_XX' + `p',1] = scalar(delta`P'_`p'plaXX)
			matrix res_mat_plaXX[`=index_XX' + `p',2] = sd_delta`P'_`p'plaXX
			matrix res_mat_plaXX[`=index_XX' + `p',3] = LB`P'_`p'plaXX
			matrix res_mat_plaXX[`=index_XX' + `p',4] = UB`P'_`p'plaXX
			matrix res_mat_plaXX[`=index_XX' + `p',5] = N_Switchers`P'_`p'plaXX
			matrix res_mat_plaXX[`=index_XX' + `p',6] = Nstayers`P'_`p'plaXX
	}
	
	}
	if (`waoss_XX' == 1&`P'==2){
		
			**********************************************************	
	matrix res_mat_XX[`=index_XX' + `p',1] = scalar(delta`P'_`p'XX)
	matrix res_mat_XX[`=index_XX' + `p',2] = sd_delta`P'_`p'XX
	matrix res_mat_XX[`=index_XX' + `p',3] = LB`P'_`p'XX
	matrix res_mat_XX[`=index_XX' + `p',4] = UB`P'_`p'XX
	matrix res_mat_XX[`=index_XX' + `p',5] = N_Switchers`P'_`p'XX
	matrix res_mat_XX[`=index_XX' + `p',6] = Nstayers`P'_`p'XX

	if ("`placebo'"!=""){

			**********************************************************	
			
			matrix res_mat_plaXX[`=index_XX' + `p',1] = scalar(delta`P'_`p'plaXX)
			matrix res_mat_plaXX[`=index_XX' + `p',2] = sd_delta`P'_`p'plaXX
			matrix res_mat_plaXX[`=index_XX' + `p',3] = LB`P'_`p'plaXX
			matrix res_mat_plaXX[`=index_XX' + `p',4] = UB`P'_`p'plaXX
			matrix res_mat_plaXX[`=index_XX' + `p',5] = N_Switchers`P'_`p'plaXX
			matrix res_mat_plaXX[`=index_XX' + `p',6] = Nstayers`P'_`p'plaXX
	}
	
	}
	if (`iwaoss_XX' == 1&`P'==3){
		
			**********************************************************	

	matrix res_mat_XX[`=index_XX' + `p',1] = scalar(delta`P'_`p'XX)
	matrix res_mat_XX[`=index_XX' + `p',2] = sd_delta`P'_`p'XX
	matrix res_mat_XX[`=index_XX' + `p',3] = LB`P'_`p'XX
	matrix res_mat_XX[`=index_XX' + `p',4] = UB`P'_`p'XX
	matrix res_mat_XX[`=index_XX' + `p',5] = N_Switchers`P'_`p'XX
	matrix res_mat_XX[`=index_XX' + `p',6] = Nstayers`P'_`p'XX

	if ("`placebo'"!=""){

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

if (scalar(P_XX)==1){
	if (`p'>1){
	local rownames_XX `rownames_XX' " aoss_`p'"
	local rownames_plaXX `rownames_plaXX' " Placebo_`p'"
	}
	else{
	local rownames_XX `rownames_XX' "AOSS"
		local rownames_plaXX `rownames_plaXX' "Placebo"
	}
}

if (scalar(P_XX)==2){
	if (`p'>1){
	local rownames_XX `rownames_XX' "waoss_`p'"
	local rownames_plaXX `rownames_plaXX' " Placebo_`p'"
	}
	else{
	local rownames_XX `rownames_XX' "WAOSS"
	local rownames_plaXX `rownames_plaXX' "Placebo"
	}
}

if (scalar(P_XX)==3){
	if (`p'>1){
	local rownames_XX `rownames_XX' "iv-waoss_`p'"
	local rownames_plaXX `rownames_plaXX' " Placebo_`p'"
	}
else{
	local rownames_XX `rownames_XX' "IV-WAOSS"
	local rownames_plaXX `rownames_plaXX' " Placebo"
	}
}
}
}

//ii. Colnames
matrix colnames res_mat_XX= "Estimate" "SE" "LB CI" "UB CI" "Switchers" "Stayers" 

//ii.. Rownames
matrix rownames res_mat_XX = `rownames_XX'

if ("`placebo'"!=""){
matrix colnames res_mat_plaXX= "Estimate" "SE" "LB CI" "UB CI" "Switchers" "Stayers" 
matrix rownames res_mat_plaXX = `rownames_plaXX'
}



***********************************Fill up the matrix: END*********************************
*********************************************************************************************

//local name = ustrupper("`estimator'")

**************************************DISPLAY TABLE: START***********************************
*********************************************************************************************

scalar max_mT = scalar(max_T)-1
display _newline
/*******************INFOS TO DISPLAY BEFORE THE TABLE*************************/


di as input _skip(34)"{hline 46}"
//Number of observations
if (`aoss_XX'==1){
	local nb_obs_XX =  scalar(Nstayers1_1XX) + scalar(N_Switchers1_1XX)
}
if (`waoss_XX'==1){
	local nb_obs_XX =  scalar(Nstayers2_1XX) + scalar(N_Switchers2_1XX)
}
if (`iwaoss_XX'==1){
	local nb_obs_XX =  scalar(Nstayers3_1XX) + scalar(N_Switchers3_1XX)
}
local nb_obs_adj_XX = strlen("`nb_obs_XX'")
	di as text _skip(34) "{it: Number of observations}"_skip(4) " =" _skip(`=17-`nb_obs_adj_XX'') "`nb_obs_XX'"
//Estimation method
if (`waoss_XX'==1){
if ("`estimation_method'" =="" | "`estimation_method'" =="ra"){
	di as text _skip(34) "{it: WAOSS Estimation method }" "   =  {it:reg. adjustment}"
}

if ("`estimation_method'" =="ps"){
di as text _skip(34) "{it: WAOSS Estimation method }" "   = {it:propensity-score}"
}

if ("`estimation_method'" =="dr"){
	di as text _skip(34) "{it: WAOSS Estimation method  }" "  = {it:   doubly-robust}"
}
}

if (`iwaoss_XX'==1){
if ("`estimation_method'" =="" | "`estimation_method'" =="ra"){
	di as text _skip(34) "{it: IV-WAOSS Estimation method}" " =  {it:reg. adjustment}"
}

if ("`estimation_method'" =="ps"){
di as text _skip(34) "{it: IV-WAOSS Estimation method}" " = {it:propensity-score}"
}

if ("`estimation_method'" =="dr"){
	di as text _skip(34) "{it: IV-WAOSS Estimation method}" " = {it:   doubly-robust}"
}
}

//Polynomial order
if ("`exact_match'"==""){
local pol_adj_XX = strlen("`order'")
di as text _skip(34) "{it: Polynomial order   }"_skip(8)"=" _skip(`=17-`pol_adj_XX'')"`order'"
}

//Type of Common trend assumption
if ("`noextrapolation'" !=""){
	if (scalar(N_drop_noextra_XX)>0){
	di as text _skip(34)  "{it: {hi:Common support*}}" _skip(12) "="  "{it: no extrapolation}"
	}
	else{
	di as text _skip(34)  "{it: {hi:Common support}}" _skip(13) "="  "{it: no extrapolation}"
			}
}

if ("`exact_match'"!=""){
	if (scalar(N_drop_noextra_XX)>0){
	di as text _skip(34)  "{it: {hi:Common support*}}" _skip(12) "=" _skip(2) "{it: exact matching}"
	}
	else{
	di as text _skip(34)  "{it: {hi:Common support}}" _skip(12) "=" _skip(2) "{it: exact matching}"
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

forvalues i = 1/3{
	
	if ("`i'"=="1"&`aoss_XX' == 1){
		//Effects
		matrix res_mat_1XX = res_mat_XX[ 1..max_T,....]

		di as input "{hline 80}"
		//di as input _skip(35) "Estimation of the `1'(s)"
		di as input _skip(26) "Average Of Switchers' Slopes (AOSS)"
		di as input "{hline 80}"
		if ("`disaggregate'" ==""){
			noisily matlist res_mat_1XX[1..1,....]
		}
		else{
			noisily matlist res_mat_1XX
		}
		//Placebos
		if ("`placebo'"!=""){
		matrix res_mat_1plaXX = res_mat_plaXX[ 1..max_T,....]

		di as input "{hline 80}"
		di as input _skip(32) "Placebo(s) AOSS"
		di as input "{hline 80}"
		if ("`disaggregate'" ==""){
			noisily matlist res_mat_1plaXX[1..1,....]
		}
		else{
			noisily matlist res_mat_1plaXX
		}
		}
	}
	
	if ("`i'"=="2"&`waoss_XX' == 1){
		
		//Effects
        matrix res_mat_2XX = res_mat_XX[max_T+1..2*max_T,....]

		di as input "{hline 80}"
		//di as input _skip(35) "Estimation of the `2'(s)"
		di as input _skip(22) "Weighted Average Of Switchers' Slopes (WAOSS)"
		di as input "{hline 80}"
		
		if ("`disaggregate'" ==""){
			noisily matlist res_mat_2XX[1..1,....]
		}
		else{
			noisily matlist res_mat_2XX
		}
		
		//Placebos
		if ("`placebo'"!=""){
			matrix res_mat_2plaXX = res_mat_plaXX[max_T+1..2*max_T,....]

			di as input "{hline 80}"
			di as input _skip(32) "Placebo(s) WAOSS"
			di as input "{hline 80}"
			
			if ("`disaggregate'" ==""){
				noisily matlist res_mat_2plaXX[1..1,....]
			}
			else{
				noisily matlist res_mat_2plaXX
			}			
		}
	}
	
	if ("`i'"=="3"&`iwaoss_XX' == 1){
		
		//Effects
        matrix res_mat_3XX = res_mat_XX[2*max_T+1...,....]

		di as input "{hline 80}"
		//di as input _skip(35) "Estimation of the `3'(s)"
		di as input _skip(22) "IV-Weighted Average Of Switchers' Slopes (IWAOSS)"
		di as input "{hline 80}"
		if ("`disaggregate'" ==""){
				noisily matlist res_mat_3XX[1..1,....]
		}
		else{
				noisily matlist res_mat_3XX
		}	
		
			//Placebos
		if ("`placebo'"!=""){
	    matrix res_mat_3plaXX = res_mat_plaXX[2*max_T+1...,....]
		di as input "{hline 80}"
		di as input _skip(30) "Placebo(s) IV-WAOSS"
		di as input "{hline 80}"
		
		if ("`disaggregate'" ==""){
				noisily matlist res_mat_3plaXX[1..1,....]
		}
		else{
				noisily matlist res_mat_3plaXX
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
if (`aoss_XX' == 1 & `waoss_XX' == 1){
	scalar ratio_sd_XX = scalar(delta1_1XX)/scalar(delta2_1XX)
	if(scalar(ratio_sd_XX)>10){
		di as error "You might have quasi-stayers in your data. The aoss estimand is likely to be biased."
	}
}

//Test results
if ("`aoss_vs_waoss'"!=""&`a_vs_w'==2){
	di as input " "

	di as input _skip(26) "Test of difference between AOSS and WAOSS"
	di as text "{it:H0: AOSS = WAOSS}"
	di as input "{hline 80}"
	noisily matlist aoss_vs_waoss_mat
}
di as input "{hline 80}"

**************************************DISPLAY TABLE: END*************************************
*********************************************************************************************

//>MAIN: Restore the inputted dataset
	restore
	
//ERETURNS
ereturn clear 

if (`aoss_XX' == 1){
ereturn scalar LB1=LB1_1XX
ereturn scalar UB1=UB1_1XX
ereturn scalar sd_AOSS = sd_delta1_1XX 
ereturn scalar AOSS = delta1_1XX
}
if (`waoss_XX' == 1){
ereturn scalar LB2=LB2_1XX
ereturn scalar UB2=UB2_1XX
ereturn scalar sd_WAOSS = sd_delta2_1XX 
ereturn scalar WAOSS = delta2_1XX
}
if (`iwaoss_XX' == 1){
ereturn scalar LB3=LB3_1XX
ereturn scalar UB3=UB3_2XX
ereturn scalar sd_IWAOSS = sd_delta3_1XX 
ereturn scalar IWAOSS = delta3_1XX
}

forvalues p = 2/`=max_T'{
	if (`aoss_XX' == 1){
	ereturn scalar LB1_`p'=LB1_`p'XX
	ereturn scalar UB1_`p'=UB1_`p'XX
	ereturn scalar aoss_`p' = delta1_`p'XX
	ereturn scalar sd_aoss_ = sd_delta1_`p'XX 
	}
	if (`waoss_XX' == 1){
	ereturn scalar LB2_`p'=LB2_`p'XX
	ereturn scalar UB2_`p'=UB2_`p'XX
	ereturn scalar waoss_`p' = delta2_`p'XX
	ereturn scalar sd_waoss_ = sd_delta2_`p'XX 
	}
	if (`iwaoss_XX' == 1){
	ereturn scalar LB3_`p'=LB3_`p'XX
	ereturn scalar UB3_`p'=UB3_`p'XX
	ereturn scalar iwaoss_`p' = delta3_`p'XX
	ereturn scalar sd_iwaoss_ = sd_delta3_`p'XX 
}
}

end

*******************************************************************************************
//Program 2 : This program compute the estimators for each two successive time periods
*******************************************************************************************

capture program drop did_multiplegt_stat_pairwise
program did_multiplegt_stat_pairwise, eclass
	version 12.0
	syntax varlist(min=4 max=5 numeric) [if] [in] [, estimator(string) ORder(integer 1) NOEXTRApolation weight(varlist numeric) switchers(string) pairwise(integer 2) data_1XX(string) aoss(integer 0) waoss(integer 0) iwaoss(integer 0) estimation_method(string) placebo exact_match cluster(varlist max=1)]
	
quietly{
//> CORE preserve
tempfile OG_dataPathcore
	save "`OG_dataPathcore'.dta", replace

//IV method:
local IV_feed_XX = "no"

if ("`5'"!=""&"`5'"!=","){
local IV_feed_XX = "yes"
}

if ("`IV_feed_XX'"=="no"&(`iwaoss' == 1|"`estimator'" == "")){
	//di as error "To compute the iwaoss you must specify the IV variable."
	local iwaoss = 0
	//exit
}

**# Bookmark #1: Format the data: Subselect the two times we are interested in
if ("`placebo'"==""){
keep if inlist(T_XX,  `pairwise'-1, `pairwise')
}
else{
keep if inlist(T_XX,  `pairwise'-2, `pairwise'-1, `pairwise')
local pla = "pla"
//di as error "inlist(T_XX,  `pairwise'-2, `pairwise'-1, `pairwise')"
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

xtset ID_XX T_XX

//Generate deltaY = Y_t - Y_(t-1)
bysort ID_XX: gen deltaY_XX = D.Y_XX
///>put it  at the same level of Y_(t-1)
if ("`placebo'"==""){
	bysort ID_XX: gegen delta_temp = mean(deltaY_XX)
	replace deltaY_XX = delta_temp
	drop delta_temp
}
else{
	gen delta_temp = deltaY_XX if T_XX == 2 // = Y_{t-2} - Y_{t-1}
	bysort ID_XX: gegen delta_temp2 = mean(delta_temp)
	replace deltaY_XX = delta_temp2
	drop delta_temp2 
	drop delta_temp
}

//Generate deltaD_t = D_t - D_(t-1)
sort ID_XX T_XX
bysort ID_XX : gen deltaD_XX = D.D_XX

if ("`placebo'"!=""&(`aoss'==1|`waoss'==1)){
	cap drop inSamplePlacebo_tempXX
	cap drop inSamplePlacebo_XX
	gen inSamplePlacebo_tempXX = (deltaD_XX==0)&(T_XX==2) //Units such that D_{t-2} = D_{t-1}
	bysort ID_XX: gegen inSamplePlacebo_XX = max(inSamplePlacebo_tempXX)
	
	//Only keep Units such that D_{t-2} = D_{t-1}
	keep if inSamplePlacebo_XX==1
	drop if T_XX == 1 //We do not need that line since we've already computed Y_{t-2} - Y_{t-1}, and selected Units such that D_{t-2} = D_{t-1} // And evrything that follows is the same as the computation of the effects:)
	bysort ID_XX: replace deltaD_XX = . if T_XX!=3 //We need the DeltaD_t only and we will take the mean after to keep the same value for all the dates
}

if (`iwaoss' == 1){
	//IV
	cap drop deltaZ_XX
	cap drop SI_XX
	cap drop Z_XX
	cap drop outOfBoundsiV_XX
	
	gen Z_XX = `5'
	
	//Generate deltaZ_t = Z_t - Z_(t-1)
	sort ID_XX T_XX
	bysort ID_XX : gen deltaZ_XX = D.Z_XX
	if ("`placebo'"!=""){	
	cap drop inSamplePlaceboIV_tempXX
	cap drop inSamplePlaceboIV_XX
	gen inSamplePlaceboIV_tempXX = (deltaZ_XX==0)&(T_XX==2) //Units such that Z_{t-2} = Z_{t-1}
	bysort ID_XX: gegen inSamplePlaceboIV_XX = max(inSamplePlaceboIV_tempXX)
	
	//Only keep Units such that Z_{t-2} = Z_{t-1}
	keep if inSamplePlaceboIV_XX==1
	drop if T_XX == 1 //We do not need that line since we've already computed Y_{t-2} - Y_{t-1}, and selected Units such that Z_{t-2} = Z_{t-1} // And evrything that follows is the same as the computation of the effects:)
	bysort ID_XX: replace deltaZ_XX = . if T_XX!=3 //We need the DeltaZ_t only and we will take the mean after to keep the same value for all the dates
	
	}
	
}

if (_N == 0){ //If the placebo cannot be estimated (because the subsample {i: D_{t-2} = D_{t-1}} is empty), create the variables,and scalars and set them to .
	if (`waoss'==1|`aoss'==1){
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

if (`iwaoss'==1){
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
if (`iwaoss' == 1){
	cap drop used_in_IV`pairwise'_XX
	gen used_in_IV`pairwise'_XX = (used_in_`pairwise'_XX==1&deltaZ_XX!=.) 
	drop if !used_in_IV`pairwise'_XX 
}
else{
	drop if !used_in_`pairwise'_XX //missing values: after the merge, below, with all the datasets that variable will be set to . for the unused units then we will exploit that to tag those units.(deprecated, I used rownonmiss instead)
}



//generate Switcher : S = 1 if switcher-up, -1 if switcher-down, 0 if stayer
gen S_XX = (deltaD_XX>0)-(deltaD_XX<0) // = S+ - S-

if (`waoss'==1|`aoss'==1){
cap drop absdeltaD_XX
gen absdeltaD_XX = S_XX*deltaD_XX

	if ("`switchers'" =="up"){
	 drop if S_XX==-1
	}
	if ("`switchers'" =="down"){
	 drop if S_XX==1
	}
}

if (`iwaoss'==1){
cap drop absdeltaZ_XX
gen absdeltaZ_XX = SI_XX*deltaZ_XX

	if ("`switchers'" =="up"){
	 drop if SI_XX==-1
	}
	if ("`switchers'" =="down"){
	 drop if SI_XX==1
	}
}

//We have all the variable we need at the first year so we can drop the 'second' year line
//  But before let's also take the right line of weights we need: W_{g,t} if we consider the cell (t-1,t)
replace weight_XX = F.weight_XX
replace weight_cXX = F.weight_cXX

if ("`placebo'"==""){
quietly drop if T_XX == 2 
}
else{
quietly drop if T_XX == 3
}

rename  D_XX D1_XX

if (_N>0){ //In this section we use bysort ..: gen . If the dataset is empty (e.g, the user specified switchers(up) and there is no switcher-up for the current pair of times),  bysort ..: gen does not displays an error and does not create the variable, then if we call the variable (which was supposed to be created) the program crashes. To avoid that, I add the if  condition (_N>0) after the subsample based on the option switchers in line 1283.
if("`noextrapolation'"!=""){
	
	if (`aoss' ==1|`waoss'==1){
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
	
	if(`iwaoss'==1){
	sum Z1_XX if SI_XX==0
	scalar max_Z1_`pla'XX = r(max)
	scalar min_Z1_`pla'XX = r(min)
	gen outOfBoundsiV_XX = (Z1_XX<scalar(min_Z1_`pla'XX)|Z1_XX>scalar(max_Z1_`pla'XX))
	
	count if outOfBoundsiV_XX==1
	scalar N_IVdrop_`pairwise'`pla'XX = r(N) //Just keep the number of switchers that violate the noextrapolation condition ; I display the nummber in the outputs of the command as a way to let the users know the consequences of the nonextra option.
	drop if outOfBoundsiV_XX==1
	if (scalar(N_IVdrop_`pairwise'`pla'XX)>0&scalar(gap_`pairwise'`pla'XX)==0&scalar(N_drop_`pairwise'`pla'XX)<_N-1){
		scalar N_drop_noextra_`pla'XX = scalar(N_drop_noextra_`pla'XX) + scalar(N_IVdrop_`pairwise'`pla'XX)
	}
	}
}
if ("`exact_match'"!=""){
	cap drop s_has_match_temp_XX
	cap drop s_has_match_XX
	
	cap drop c_has_match_temp_XX
	cap drop c_has_match_XX
	
		if (`aoss' ==1|`waoss'==1){
		gegen s_has_match_temp_XX = min(absdeltaD_XX), by(D1_XX)
		gen s_has_match_XX = (s_has_match_temp_XX==0)
		
		gegen c_has_match_temp_XX = max(absdeltaD_XX) , by(D1_XX)
		gen c_has_match_XX = (c_has_match_temp_XX>0)
		
		replace s_has_match_XX=. if S_XX==0 
		replace c_has_match_XX=. if S_XX!=0 
		
			count if s_has_match_XX==0
	        scalar N_drop_`pairwise'`pla'XX = r(N)
			
			count if c_has_match_XX==0
	        scalar N_drop_c_`pairwise'`pla'XX = r(N)
		}

		if (`iwaoss' ==1){
		gegen s_has_match_temp_XX = min(absdeltaZ_XX), by(Z1_XX)
		gen s_has_match_XX = (s_has_match_temp_XX==0)&SI_XX!=0
		
		gegen c_has_match_temp_XX = max(absdeltaZ_XX), by(Z1_XX)
		gen c_has_match_XX = (c_has_match_temp_XX>0)&SI_XX==0
		
			count if s_has_match_XX==0
	        scalar N_drop_`pairwise'`pla'XX = r(N)
			
			count if c_has_match_XX==0
	        scalar N_drop_c_`pairwise'`pla'XX = r(N)
		}
		

	drop if s_has_match_XX==0 //
	//drop if c_has_match_XX==0 //? Discuss with Clément

	if (scalar(N_drop_`pairwise'`pla'XX)>0&scalar(gap_`pairwise'`pla'XX)==0){
	scalar N_drop_noextra_`pla'XX = scalar(N_drop_noextra_`pla'XX) + scalar(N_drop_`pairwise'`pla'XX)
	}
	
	if (scalar(N_drop_c_`pairwise'`pla'XX)>0&scalar(gap_`pairwise'`pla'XX)==0){
	scalar N_drop_c_noextra_`pla'XX = scalar(N_drop_c_noextra_`pla'XX) + scalar(N_drop_c_`pairwise'`pla'XX)
	}
}

}
/*********************************
Some scalars we need below:
*********************************/
sum weight_XX
scalar W_`pla'XX = r(sum)
//scalar N_`pla'XX = _N

*******Here I handle two related problems: Panel with gaps (using tsfilled_XX) and cases where we have only switchers or only stayers (using count)

if (`waoss'==1|`aoss'==1){
// only switchers or only stayers: I did add the index `pairwise' since I want to use it if either aoss or waoss is requested.

count if S_XX!=0
scalar n_switchers_`pla'XX = r(N) 

count if S_XX==0
scalar n_stayers_`pla'XX = r(N)

//Numbers to display*****************************************************
scalar Nstayers1_`pairwise'`pla'XX  = scalar(n_stayers_`pla'XX)
scalar Nstayers2_`pairwise'`pla'XX  = scalar(n_stayers_`pla'XX)

scalar N_Switchers1_`pairwise'`pla'XX = scalar(n_switchers_`pla'XX)
scalar N_Switchers2_`pairwise'`pla'XX = scalar(n_switchers_`pla'XX)
}

if (`iwaoss'==1){
count if SI_XX!=0
scalar n_switchersIV_`pla'XX = r(N) 

count if SI_XX==0
scalar n_stayersIV_`pla'XX = r(N)

scalar N_Switchers3_`pairwise'`pla'XX = scalar(n_switchersIV_`pla'XX)
scalar Nstayers3_`pairwise'`pla'XX  = scalar(n_stayersIV_`pla'XX)

}
**************************************************************************	
	if ("`exact_match'"!=""){ //Take the number of distinct values of the baseline treatment: Question for Clement: I did it for each pair of periods and not on the entire sample.
		levelsof D1_XX
		local order = r(r)
	}
		
if (`waoss' == 1 | `aoss' == 1 ){	
if (scalar(gap_`pairwise'`pla'XX)==0&scalar(n_switchers_`pla'XX)>0&scalar(n_stayers_`pla'XX)>1){ //Start of feasible estimation //I Need to do it for the IV as well.

**# Bookmark #0 Preliminaries
	cap drop predicted_XX
	cap drop mean_pred_XX
	cap drop trimmed_out_XX
	cap drop inner_sumdelta1_XX

    cap drop ESbis_XX_D1
	cap drop ES_XX_D1

// Generating polynomials of the baseline treatement	
		//i for D
			local vars_pol_XX = ""
			forvalues pol_level = 1/`order'{
			scalar pol_level_`pla'XX = `pol_level'
			capture drop D1_XX_`pol_level'_XX 
			gen D1_XX_`pol_level'_XX = D1_XX^scalar(pol_level_`pla'XX)
			local vars_pol_XX = "`vars_pol_XX' D1_XX_`pol_level'_XX"
			}
				//Generating the binary S
				cap drop Sbis_XX
				gen Sbis_XX = (S_XX!=0&S_XX!=.) //this Sbis_XX is to be used for \delta_1, and for \delta_2 when we do not need to distinguish switchers-up from switchers-down where the only matter is being switcher or not.
	
/*******************************************************************************
Perfom here the main logit regressions that are needed for the three estimators
*******************************************************************************/

	//*********************for AOSS, WAOSS

	// Performing the regression (polynomial series) estimation to estimate \hat{E}(deltaY|D1, S=0)
	
	     // \hat{E}(deltaY|D1, S=0)
	reg deltaY_XX `vars_pol_XX' [iweight=weight_XX] if S_XX==0 //This regression can generated an error when the polynomial order is very high and lead to values >  1e+38 (the upper bound of float in stata.).
	predict mean_pred_XX , xb 
	
		 // deltaY_i - \hat{E}(deltaY|D_{1i}|S = 0)
	gen inner_sumdelta12_XX  = deltaY_XX - mean_pred_XX //WILL BE USED FOR AOSS AND WAOSS AS WELL

// 1. Estimate P(S = 0|D_1) 
	cap drop S0_XX
	gen S0_XX = 1-Sbis_XX
    if ("`exact_match'"==""){
	capture logit S0_XX `vars_pol_XX' [iweight=weight_XX], asis
	if (_rc==430){
	//di as error "Warning: convergence not achieved." //To be dropped after?
	}	
	predict PS0D1_XX, pr asif
	//Convention Logit STATA R to match
	replace PS0D1_XX=0 if PS0D1_XX<=10^(-10)
	}
	else{ 		//This is for the IF and point estimate of the waoss when we have discrete treatment, instead of using logit use regressions
	
		//Estimation of 1-E(S|D1) = P(S = 0|D_1) in exact_match case
		reg Sbis_XX `vars_pol_XX' [iweight=weight_XX]
		predict ESbis_XX_D1, xb
		
		//Estimation of  \hat{E}(S+-S-|D1) for both \Phi_2 : Remember S_XX = S+ - S-, i.e (deltaD_XX>0)-(deltaD_XX<0) in line 79,  so do:
		reg S_XX `vars_pol_XX' [iweight=weight_XX]
		predict ES_XX_D1 , xb  

	}

	//2. Estimate P(S=0)
	sum S0_XX [iweight=weight_XX]
	scalar PS0_`pla'XX = r(mean) //scalar(Nstayers2_`pairwise'XX )/(scalar(N_Switchers2_`pairwise'XX)+ scalar(Nstayers2_`pairwise'XX))
	
	//3. P(S+=1|D_1), P(S-=1|D_1), P(S+=1) and P(S-=1) are generated after.

	//*********************for iWAOSS: It is done in Bookmark #3
	

********************************************************************************
**# Bookmark #1 AOSS
********************************************************************************

if (`aoss' == 1){
	
************************************************	
	cap drop S_over_deltaD_XX
	cap drop meanS_over_deltaD_XX
	
	cap drop Phi1_`pairwise'`pla'XX //This influence function Phi1_{1,t}
************************************************	
	// 0) Compute P_t = P(S_t = 1) = E(S_t) for the aggregation afterward
	sum Sbis_XX [iweight=weight_XX]
	//For aggregation
	scalar P_`pairwise'`pla'XX = r(mean) 
	scalar PS_sum_`pla'XX = scalar(PS_sum_`pla'XX) + scalar(P_`pairwise'`pla'XX) //PS_sum is initialized outside of this program
	scalar ES_`pla'XX = r(mean)  

	// 1) Compute \hat{delta}_1
	gen inner_sumdelta1_XX  = inner_sumdelta12_XX/deltaD_XX 
	sum inner_sumdelta1_XX [iweight=weight_XX]
	scalar delta1_`pairwise'`pla'XX = r(mean)

	// 2) Compute the variance of \hat{delta}_1
	//sum Sbis_XX 
	//scalar ES_XX = r(mean)
	gen S_over_deltaD_XX = Sbis_XX/deltaD_XX 
	//The convention 0/0 = 0
	replace S_over_deltaD_XX =0 if Sbis_XX==0
	
		//i. estimation of  \hat{E}(S/deltaD|D1)
	reg S_over_deltaD_XX `vars_pol_XX' [iweight=weight_XX]
	predict meanS_over_deltaD_XX , xb  
	
	//Doulo: The Influence function is indexed by t to ease the aggregation after the loop is over by using successive merging of datasets
	
	if ("`exact_match'"==""){
	gen Phi1_`pairwise'`pla'XX  = (S_over_deltaD_XX - meanS_over_deltaD_XX*(1-Sbis_XX)/(PS0D1_XX))*inner_sumdelta12_XX //Here
	}
	else{
	gen Phi1_`pairwise'`pla'XX  = (S_over_deltaD_XX - meanS_over_deltaD_XX*(1-Sbis_XX)/(1-ESbis_XX_D1))*inner_sumdelta12_XX 
	}
	replace Phi1_`pairwise'`pla'XX  =[Phi1_`pairwise'`pla'XX - scalar(delta1_`pairwise'`pla'XX)*Sbis_XX]/scalar(ES_`pla'XX)
	sum Phi1_`pairwise'`pla'XX [iweight=weight_XX]
	scalar mean_IF1_`pairwise'`pla' = r(mean) //check if the mean is close to zero when I will output the simulations (to be drop in the final version)
	
	if ("`cluster'"!=""){ // Clustering the variance //CLUSTER OPTION //WEIGHT OPTION
		cap drop Phi1_`pairwise'`pla'_cXX //I create a new variable, because I need the one without clustering in the aggregation
		bysort `cluster': egen Phi1_`pairwise'`pla'_cXX = total(Phi1_`pairwise'`pla'XX)
		bysort `cluster': replace Phi1_`pairwise'`pla'_cXX=. if _n!=1
		sum Phi1_`pairwise'`pla'_cXX [iweight=weight_cXX] //we use the weight of the cluster here
		
	}

	scalar sd_delta1_`pairwise'`pla'XX = r(sd)/sqrt(r(sum_w))  //sqrt(r(N)) //sqrt(scalar(N_`pla'XX)) take r(sum_w) because of weights //WEIGHT OPTION
	
	scalar LB1_`pairwise'`pla'XX = scalar(delta1_`pairwise'`pla'XX) - 1.96*scalar(sd_delta1_`pairwise'`pla'XX)
	scalar UB1_`pairwise'`pla'XX = scalar(delta1_`pairwise'`pla'XX) + 1.96*scalar(sd_delta1_`pairwise'`pla'XX)
	
	//Now Let's store S_t for the Influence function variable of the aggregation
	cap drop S_`pairwise'`pla'XX
	gen S_`pairwise'`pla'XX = Sbis_XX
	
}

********************************************************************************
**# Bookmark #2 WAOSS
********************************************************************************
if (`waoss' == 1){
	
************************************************	
//cap drop absdeltaD_XX
//gen absdeltaD_XX = S_XX*deltaD_XX
	
	sum absdeltaD_XX [iweight=weight_XX]
	scalar EabsdeltaD_`pla'XX = r(mean)
	
	//For aggregation of the point estimates
	scalar EabsdeltaD_`pairwise'`pla'XX = r(mean) 
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
       i. COMPUTING THE contribution-WEIGHTS (wPlus and wMinus)
**************************************************************************/
		cap drop prod_sgndeltaDdeltaD_XX
		gen prod_sgndeltaDdeltaD_XX = S_XX*deltaD_XX
		sum prod_sgndeltaDdeltaD_XX [iweight=weight_XX] if Ster_XX==1
		//scalar w`suffix'_`pairwise'`pla'XX = r(sum)/scalar(N_`pla'XX)
		scalar w`suffix'_`pairwise'`pla'XX = r(sum)/scalar(W_`pla'XX) // because of the weight option 
//The sum at the denominator of \hat{\delta}_2`suffix'

		//use r(sum) instead of r(mean) since in case there is no observation r(mean) gives ., wheras r(sum) will give 0: this is for the r-based approach.
		sum deltaD_XX [iweight=weight_XX]  if Ster_XX==1
		scalar denom_delta2`suffix'_`pairwise'`pla'XX = r(sum)
		

/**************************************************************************
		           IF REGRESSION-BASED APPROACH 
**************************************************************************/
		if ("`estimation_method'" == ""|"`estimation_method'" == "ra"){
		
		if (denom_delta2`suffix'_`pairwise'`pla'XX ==0){
			scalar denom_delta2`suffix'_`pairwise'`pla'XX = 1 //in case it is zero set it to 1 to avoid dividing by 0, in that case the numerator is also equal to 0
		}
		
		sum  inner_sumdelta12_XX [iweight=weight_XX]  if Ster_XX==1
		scalar num_delta2`suffix'_`pairwise'`pla'XX = r(sum)
		
		scalar delta2`suffix'_`pairwise'`pla'XX = scalar(num_delta2`suffix'_`pairwise'`pla'XX)/scalar(denom_delta2`suffix'_`pairwise'`pla'XX)
		}
		
/**********************************************************************************
	  o. Preliminaries: All the variables needed regardless the estimation_method
**********************************************************************************/
	       cap drop  PS1`suffix'D1_XX
		   
	       //1. Estimate P(S`suffix'=1)
		   count if Ster_XX ==1
		   scalar nb_Switchers`suffix'`pla'XX = r(N) // I need the non-weighted number of switchers to display
		   
		   //scalar PS`suffix'1`pla'XX = scalar(nb_Switchers`suffix'`pla'XX)/scalar(N_`pla'XX)	//because of weights I do as following: //WEIGHT OPTION
		   
		   sum Ster_XX [iweight=weight_XX] if Ster_XX==1
		   scalar PS`suffix'1`pla'XX = r(sum_w)/scalar(W_`pla'XX)	
		   
if ("`exact_match'"==""){ //We only do the logit regression if we have continuous treatment, i.e., when exact_match is not specified	
		   if (scalar(PS`suffix'1`pla'XX)==0){ //I do the regression iff there is at least one switcher up/down.
		   scalar delta2`suffix'_`pairwise'`pla'XX  = 0 // the weights as well -see above-is set to 0 if {i: Ster_XX==1} = empty.
		   
		   cap drop PS1`suffix'D1_XX //create it and set it to zero, since I will call it outside this loop for the dr method.
		   gen PS1`suffix'D1_XX = 0
		   }
		   else{ 
		   	//2. Estimate P(S`suffix' = 1|D_1) 
			capture logit Ster_XX `vars_pol_XX' [iweight=weight_XX] , asis
			if (_rc==430){
				//di as error "Warning: convergence not achieved."
			}
			
			predict PS1`suffix'D1_XX //, pr asif
			//Convention Logit STATA R
			replace PS1`suffix'D1_XX=0 if PS1`suffix'D1_XX<=10^(-10)
			
		
/**************************************************************************
			               IF PS-based APPROACH
**************************************************************************/
		if ("`estimation_method'" == "ps"){
			//3. Compute deltaY*[(PS1`suffix'D1_XX*PS0_XX)/(PS0D1_XX*PS`suffix'1XX)]
			cap drop deltaYP_`suffix'XX
			gen deltaYP_`suffix'XX = deltaY_XX*(PS1`suffix'D1_XX/PS0D1_XX)*(scalar(PS0_XX)/scalar(PS`suffix'1XX))
			sum deltaYP_`suffix'XX [iweight=weight_XX] if S_XX==0
			scalar mean_deltaYP_`suffix'`pla'XX = r(mean)
			
			sum deltaY_XX [iweight=weight_XX]  if Ster_XX ==1
			scalar mean_deltaY_`pla'XX = r(mean)
			
			sum deltaD_XX [iweight=weight_XX]  if Ster_XX==1
			
			scalar delta2`suffix'_`pairwise'`pla'XX = (scalar(mean_deltaY_`pla'XX) - scalar(mean_deltaYP_`suffix'`pla'XX))/r(mean)
		}
		} //End of the else
} //End of the if with exact_match
	} //End of the suffix loop

/**************************************************************************
      ii. COMPUTING THE FINAL WEIGHTS WPLus = wPlus/(wPlus+wMinus)
**************************************************************************/
if ("`estimation_method'" == ""|"`estimation_method'" == "ps"|"`estimation_method'" == "ra"){
	scalar W_Plus_`pairwise'`pla'XX = scalar(wPlus_`pairwise'`pla'XX)/(scalar(wPlus_`pairwise'`pla'XX)+scalar(wMinus_`pairwise'`pla'XX))
}
	
	
/**************************************************************************
                       COMPUTING THE POINT ESTIMATE: END
**************************************************************************/
if ("`exact_match'"==""){
	    cap drop dr_deltaY_XX
	    gen dr_deltaY_XX = (S_XX - [(PS1PlusD1_XX - PS1MinusD1_XX)/PS0D1_XX]*(1-Sbis_XX))*inner_sumdelta12_XX
	
		sum dr_deltaY_XX [iweight=weight_XX]  //WIll use it for the dr point estimate and for the estimation of the variance!
		scalar denom_dr_delta2_`pla'XX = r(sum)
}		
		if ("`estimation_method'" == ""|"`estimation_method'" == "ps"|"`estimation_method'" == "ra"){
		scalar delta2_`pairwise'`pla'XX = scalar(W_Plus_`pairwise'`pla'XX)*scalar(delta2Plus_`pairwise'`pla'XX) + (1 - scalar(W_Plus_`pairwise'`pla'XX))*scalar(delta2Minus_`pairwise'`pla'XX ) 
		}
	
		if ("`estimation_method'" == "dr"){
		sum absdeltaD_XX [iweight=weight_XX] 
		scalar delta2_`pairwise'`pla'XX = scalar(denom_dr_delta2_`pla'XX)/r(sum)
		}
	
/**************************************************************************
          2. COMPUTING THE VARIANCE (The variance is not method-specific) // but we use linear regression if exact_match, and logit otherwise
**************************************************************************/
        if ("`exact_match'"==""){
		gen Phi2_`pairwise'`pla'XX = (dr_deltaY_XX -scalar(delta2_`pairwise'`pla'XX)*absdeltaD_XX)/scalar(EabsdeltaD_`pla'XX)
		}
		else{
		gen Phi2_`pairwise'`pla'XX = [(S_XX - ES_XX_D1*(1-Sbis_XX)/(1-ESbis_XX_D1))*inner_sumdelta12_XX -scalar(delta2_`pairwise'`pla'XX)*absdeltaD_XX]/scalar(EabsdeltaD_`pla'XX)
		}
		//save "dr_`pairwise'.dta", replace
		sum  Phi2_`pairwise'`pla'XX [iweight=weight_XX] 
		scalar mean_IF2_`pairwise'`pla' = r(mean) //check if the mean if close to zero when I will output the simulations (to be drop in the final version)

		if ("`cluster'"!=""){ // Clustering the variance
		cap drop Phi2_`pairwise'`pla'_cXX //I create a new variable, because I need the one without clustering in the aggregation
		bysort `cluster': egen Phi2_`pairwise'`pla'_cXX = total(Phi2_`pairwise'`pla'XX)
		bysort `cluster': replace Phi2_`pairwise'`pla'_cXX=. if _n!=1
		sum Phi2_`pairwise'`pla'_cXX [iweight=weight_cXX]   //we use the weight of the cluster here
		}
	
		scalar sd_delta2_`pairwise'`pla'XX = r(sd)/sqrt(r(sum_w))  // sqrt(scalar(N_`pla'XX)) // sqrt(r(sum_w)) instead of r(N)
		scalar LB2_`pairwise'`pla'XX = scalar(delta2_`pairwise'`pla'XX) - 1.96*scalar(sd_delta2_`pairwise'`pla'XX)
		scalar UB2_`pairwise'`pla'XX = scalar(delta2_`pairwise'`pla'XX) + 1.96*scalar(sd_delta2_`pairwise'`pla'XX)
		
		//Now Let's store absdeltaD_t for the Influence function variable of the aggregated point estimate
		cap drop absdeltaD_`pairwise'`pla'XX
		gen absdeltaD_`pairwise'`pla'XX = absdeltaD_XX
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
	gen absdeltaD_`pairwise'`pla'XX = .
	gen S_`pairwise'`pla'XX = .
	scalar EabsdeltaD_`pairwise'`pla'XX = 0
	scalar P_`pairwise'`pla'XX = 0
}
}
********************************************************************************
**# Bookmark #3 IWAOSS
********************************************************************************
if (`iwaoss' == 1){
if (scalar(gap_`pairwise'`pla'XX)==0&scalar(n_switchersIV_`pla'XX)>0&scalar(n_stayersIV_`pla'XX)>1){ //Start of IV feasible estimation
************************************************

		//polynamial for Z if IV requested
		
		if (`iwaoss' == 1){
			local varsIV_pol_XX = ""
			forvalues pol_level = 1/`order'{
			scalar pol_level_`pla'XX = `pol_level'
			capture drop Z1_XX_`pol_level'_XX 
			gen Z1_XX_`pol_level'_XX = Z1_XX^scalar(pol_level_`pla'XX)
			local varsIV_pol_XX = "`varsIV_pol_XX' Z1_XX_`pol_level'_XX"
			}
		}
		
cap drop innerSumIV_num_XX
//cap drop absdeltaZ_XX

cap drop SIbis_XX
cap drop SIPlus_XX
cap drop SIMinus_XX

	//gen absdeltaZ_XX = SI_XX*deltaZ_XX
	sum absdeltaZ_XX [iweight=weight_XX] 
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
	capture logit S_IV0_XX `varsIV_pol_XX' [iweight=weight_XX], asis
	if (_rc==430){
				//di as error "Warning: convergence not achieved."
			}
			
	predict PS_IV0Z1_XX, pr asif
	//Convention Logit STATA R
	replace PS_IV0Z1_XX=0 if PS_IV0Z1_XX<=10^(-10)
}
else{ 		//This is for the IF and point estimate of the waoss when we have discrete treatment, instead of using logit use regressions
	
		//Estimation of 1-E(SI|Z1) = P(SI = 0|Z_1) in exact_match case
		reg SIbis_XX `varsIV_pol_XX' [iweight=weight_XX]
		predict ESIbis_XX_Z1, xb
		//Estimation of  \hat{E}(SI+-SI-|Z1) 
		reg SI_XX `varsIV_pol_XX' [iweight=weight_XX]
		predict ESI_XX_Z1 , xb  
	}	
	//2. P(SI=0) //For PS and DR-BASED APPROACHES
	sum S_IV0_XX [iweight=weight_XX] 
	scalar PS_IV0_`pla'XX = r(mean)
	
	//3. P(SI+=1|Z_1), P(SI-=1|Z_1) (For \hat{E}(SI+-SI-|Z1)), P(SI+=1) and P(SI-=1) //For the influence function, PS and DR

       cap drop  PSIPlus1Z1_XX
	   cap drop  PSIMinus1Z1_XX
		   
	   foreach suffix in  "Minus" "Plus"{
	   	cap drop  PSI`suffix'1Z1_XX
	       //1. Estimate P(SI`suffix'=1)
		   count if SI`suffix'_XX ==1
		   scalar nb_SwitchersI`suffix'`pla'XX = r(N) 
		   //scalar PSI`suffix'1`pla'XX = scalar(nb_SwitchersI`suffix'`pla'XX)/scalar(N_`pla'XX)	 //P(SI+=1) and P(SI-=1) ////because of weights I do as following: //WEIGHT OPTION
		   sum SI`suffix'_XX [iweight=weight_XX] if SI`suffix'_XX ==1
		   scalar PSI`suffix'1`pla'XX = r(sum_w)/scalar(W_`pla'XX)
		   
		   if (scalar(PSI`suffix'1`pla'XX)==0){ //I do the regression iff there is at least one switcher up/down.
		   		   
             //create it and set it to zero, since I will call it outside this loop for the dr method.
		   gen PSI`suffix'1Z1_XX = 0
		   }
		   else{ 
			if ("`exact_match'"==""){
			capture logit SI`suffix'_XX `varsIV_pol_XX' [iweight=weight_XX] , asis
			if (_rc==430){
				//di as error "Warning: convergence not achieved."
			}
			
			predict PSI`suffix'1Z1_XX, pr asif //P(SI+=1|Z_1) and P(SI-=1|Z_1)
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
	reg deltaY_XX `varsIV_pol_XX' [iweight=weight_XX] if SI_XX==0
	predict meandeltaY_predIV_XX , xb 
	//scalar Nstayers3_`pairwise'XX  = e(N) 
	
	cap drop innerSumIV_num_XX
	gen innerSumIV_num_XX = deltaY_XX - meandeltaY_predIV_XX
	
	//ii. Estimation of \hat{E}(deltaD|Z1, SI=0)
	cap drop meandeltaD_predIV_XX
	reg deltaD_XX `varsIV_pol_XX' [iweight=weight_XX] if SI_XX==0
	predict meandeltaD_predIV_XX , xb 
	
	cap drop innerSumIV_denom_`pairwise'`pla'XX
	gen innerSumIV_denom_`pairwise'`pla'XX = deltaD_XX - meandeltaD_predIV_XX
	
/**************************************************************************
		           1.1 IF REGRESSION-BASED APPROACH 
**************************************************************************/
if ("`estimation_method'" == ""|"`estimation_method'" == "ra"){
	
	replace innerSumIV_num_XX = SI_XX*innerSumIV_num_XX
	sum innerSumIV_num_XX [iweight=weight_XX] 
	scalar num_deltaIV`pairwise'`pla'XX = r(mean)
 
    replace innerSumIV_denom_`pairwise'`pla'XX = SI_XX*innerSumIV_denom_`pairwise'`pla'XX
	sum innerSumIV_denom_`pairwise'`pla'XX [iweight=weight_XX] 
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
			 sum deltaYP_IVXX [iweight=weight_XX]  if SIbis_XX==0
			 scalar mean_deltaYP_IV`pla'XX = r(mean)
			 
			 //1.2 Compute sgn(deltaZ)*deltaY = SI_XX*deltaY_XX: Remember SI_XX = (deltaZ_XX>0) - (deltaZ_XX<0)
			 cap drop prod_sgndeltaZdeltaY_XX
			 gen prod_sgndeltaZdeltaY_XX = SI_XX*deltaY_XX
			 sum prod_sgndeltaZdeltaY_XX [iweight=weight_XX] 
			 scalar mean_sgndeltaZdeltaY_`pla'XX = r(mean)
			 
			 //1.3
			 scalar num_deltaIV`pairwise'`pla'XX = scalar(mean_sgndeltaZdeltaY_`pla'XX) - scalar(mean_deltaYP_IV`pla'XX)

			//2. The denominator	
			
		     //2.1. Compute deltaD*[(PSIPlus1Z1_XX-PSIMinus1Z1_XX)*PS_IV0_XX)/(PS_IV0Z1_XX)]
			 cap drop deltaDP_IVXX
			 gen deltaDP_IVXX = deltaD_XX*[(PSIPlus1Z1_XX-PSIMinus1Z1_XX)/PS_IV0Z1_XX*(scalar(PS_IV0_`pla'XX))]
			 sum deltaDP_IVXX [iweight=weight_XX]  if SIbis_XX==0
			 scalar mean_deltaDP_IV`pla'XX = r(mean)	
			 
			//2.2 Compute sgn(deltaZ)*deltaD = SI_XX*deltaD_XX
			 cap drop prod_sgndeltaZdeltaD_XX
			 gen prod_sgndeltaZdeltaD_XX = SI_XX*deltaD_XX
			 sum prod_sgndeltaZdeltaD_XX [iweight=weight_XX] 
			 scalar mean_sgndeltaZdeltaD_`pla'XX = r(mean)	
			 
		    //2.3
			scalar denom_deltaIV`pairwise'`pla'XX = scalar(mean_sgndeltaZdeltaD_`pla'XX) - scalar(mean_deltaDP_IV`pla'XX)
			}
		
		if ("`estimation_method'" == "dr"){
			
			//1. The numerator	
			cap drop dr_IVdeltaY_XX
	        gen dr_IVdeltaY_XX = (SI_XX - [(PSIPlus1Z1_XX - PSIMinus1Z1_XX)/PS_IV0Z1_XX]*(1-SIbis_XX))*innerSumIV_num_XX
			
			sum dr_IVdeltaY_XX [iweight=weight_XX] 
			scalar num_deltaIV`pairwise'`pla'XX = r(mean) 
			
			//2. The denominator	
			cap drop dr_IVdeltaD_XX
	        gen dr_IVdeltaD_XX = (SI_XX - [(PSIPlus1Z1_XX - PSIMinus1Z1_XX)/PS_IV0Z1_XX]*(1-SIbis_XX))*innerSumIV_denom_`pairwise'`pla'XX
			
			sum dr_IVdeltaD_XX [iweight=weight_XX] 
			scalar denom_deltaIV`pairwise'`pla'XX = r(mean)
	

		}

 
 //Compute the point estimate by scalar(num_deltaIV)/scalar(denom_deltaIV)
 scalar delta3_`pairwise'`pla'XX = scalar(num_deltaIV`pairwise'`pla'XX)/scalar(denom_deltaIV`pairwise'`pla'XX)
 
 //For aggreagation
 scalar denom_deltaIV_sum_`pla'XX = scalar(denom_deltaIV_sum_`pla'XX ) + scalar(denom_deltaIV`pairwise'`pla'XX) //denom_deltaIV_sum_`pla'XX is initialized outside of this program

 /**************************************************************************
            2. COMPUTING the variance of \hat{delta}_IV: START
**************************************************************************/
	
	 //i. Compute phi_Y
	 sum innerSumIV_num_XX [iweight=weight_XX] 
	 scalar delta_Y_`pairwise'`pla'XX = r(mean)
	 //scalar delta_Y_`pairwise'`pla'XX = r(mean)/scalar(EabsdeltaZ_`pairwise'`pla'XX)

	 //reg deltaY_XX `vars_pol_XX' [iweight=weight_XX] if SI_XX==0 
	 reg deltaY_XX `varsIV_pol_XX' [iweight=weight_XX] if SI_XX==0 // meandeltaY_predIV_XX
	 predict mean_pred_Y_IV_XX , xb 
	 
	 if ("`exact_match'"==""){
	 gen Phi_Y_XX = (SI_XX - (PSIPlus1Z1_XX-PSIMinus1Z1_XX)*(1-SIbis_XX)/(PS_IV0Z1_XX))*(deltaY_XX - mean_pred_Y_IV_XX) - scalar(delta_Y_`pairwise'`pla'XX)*absdeltaZ_XX
	 }
	 else{
	 gen Phi_Y_XX = (SI_XX - (ESI_XX_Z1)*(1-SIbis_XX)/(1-ESIbis_XX_Z1))*(deltaY_XX - mean_pred_Y_IV_XX) - scalar(delta_Y_`pairwise'`pla'XX)*absdeltaZ_XX
	 }
	 replace Phi_Y_XX = Phi_Y_XX/scalar(EabsdeltaZ_`pairwise'`pla'XX)

	 //ii. Compute phi_D
	 sum innerSumIV_denom_`pairwise'`pla'XX  [iweight=weight_XX] 
	 scalar delta_D_`pairwise'`pla'XX = r(mean)

	 
	 //reg deltaD_XX `vars_pol_XX' [iweight=weight_XX] if SI_XX==0 
	 reg deltaD_XX `varsIV_pol_XX' [iweight=weight_XX] if SI_XX==0 
	 predict mean_pred_D_IV_XX , xb 
	 
	 if ("`exact_match'"==""){	 
	 gen Phi_D_XX = (SI_XX - (PSIPlus1Z1_XX-PSIMinus1Z1_XX)*(1-SIbis_XX)/(PS_IV0Z1_XX))*(deltaD_XX - mean_pred_D_IV_XX) - scalar(delta_D_`pairwise'`pla'XX )*absdeltaZ_XX
	 }
	 else{
	 gen Phi_D_XX = (SI_XX - (ESI_XX_Z1)*(1-SIbis_XX)/(1-ESIbis_XX_Z1))*(deltaD_XX - mean_pred_D_IV_XX) - scalar(delta_D_`pairwise'`pla'XX )*absdeltaZ_XX 	
	 }
	 replace Phi_D_XX = Phi_D_XX/scalar(EabsdeltaZ_`pairwise'`pla'XX)
	 
	 //iii. Now compute Phi_IV
	 gen Phi3_`pairwise'`pla'XX = (Phi_Y_XX - scalar(delta3_`pairwise'`pla'XX)*Phi_D_XX)/scalar(delta_D_`pairwise'`pla'XX )
	 
	sum Phi3_`pairwise'`pla'XX [iweight=weight_XX] 
	scalar mean_IF3`pairwise'`pla' = r(mean) //check if the mean is close to zero when I will output the simulations (to be drop in the final version)
	
	if ("`cluster'"!=""){ // Clustering the variance //CLUSTER OPTION
		cap drop Phi3_`pairwise'`pla'_cXX //I create a new variable, because I need the one without clustering in the aggregation
		bysort `cluster': egen Phi3_`pairwise'`pla'_cXX = total(Phi3_`pairwise'`pla'XX)
		bysort `cluster': replace Phi3_`pairwise'`pla'_cXX=. if _n!=1
		sum Phi3_`pairwise'`pla'_cXX [iweight=weight_cXX]   //we use the weight of the cluster here
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
		
	//If we are in only one type cases
		if (scalar(n_stayersIV_XX)<2){
		scalar N_Switchers3_`pairwise'`pla'XX = _N - scalar(n_stayersIV_XX)
		scalar Nstayers3_`pairwise'`pla'XX = scalar(n_stayersIV_XX)
		}	
		
		if (scalar(n_switchersIV_XX)==0){
		scalar N_Switchers3_`pairwise'`pla'XX = 0
		scalar Nstayers3_`pairwise'`pla'XX = _N
		}	
}
} 
} //End of else from if (_N==0)

sort ID_XX
///////I will merge the datasets here! This is to compute the aggregated influence function
//di as error "Here `pairwise':`data_1XX'"

cap drop absdeltaD_errorXX //This in case waoss is not requested: just to avoid doing many ifs.
gen absdeltaD_errorXX=.
cap drop S_errorXX 
gen S_errorXX=.
cap drop innerSumIV_denom_errorXX
gen innerSumIV_denom_errorXX = .
//Keep ID_XX, Phi1_t, Phi2_t, S_t, and absdeltaD_t, P_`pairwise'XX
keep ID_XX Phi?_*XX S_*XX absdeltaD_*XX used_in_*_XX innerSumIV_denom_*XX `cluster' weight_XX weight_cXX //Phi* //Ster_XX ESbis_XX_D1 meanS_over_deltaD_XX inner_sumdelta12_XX mean_pred_XX deltaY_XX deltaD_XX outOfBounds_XX
drop absdeltaD_errorXX
drop S_errorXX
drop innerSumIV_denom_errorXX

merge 1:1 ID_XX using "`data_1XX'.dta", gen(merge_`pairwise'`pla'XX)

//Note Doulo: The merge_`pairwise'XX variable will be very usefull if we need afterward to give information about which units are used in each DID when for instance we have unbalanced panel.

tempfile data_1XX
save "`data_1XX'.dta", replace

glob data_1XX = "`data_1XX'"

//> CORE restore
use "`OG_dataPathcore'.dta", clear

}
//End of quietly

	//Doulo: I stopped here
end

///////Program dropping a specific list of scalars: To use after
cap program drop scalars_to_drop
program define scalars_to_drop
args to_keep
 di as error "`args'"
gen tag_XX = 1

mata: to_erase = st_dir("global", "numscalar", "*_XX")
getmata (to_erase*) = to_erase, force
qui levelsof to_erase, local(scalars) 

foreach l of local scalars {
	local tester = strpos("`to_keep'", "`l'")
	if (`tester'==0){
	scalar drop `l'
	di as error "`l' is dropped"
	}
}
drop if tag_XX != 1
drop to_erase* tag_XX
end