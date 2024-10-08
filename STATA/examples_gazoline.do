cap log close
log using "Results_gazoline", replace
use "gazoline_did_multiplegt_stat.dta", clear


*******************************************************************************
cap prog drop did_multiplegt_stat
qui do "did_multiplegt_stat.ado"

/*******************************************************************************STATA version 
*******************************************************************************/
//Panel A: Reduced-form effect of taxes on quantities consumed.

//1. Linear model
//describe, short
//set trace on
did_multiplegt_stat lngca id year tau, or(1 2 3 4 6) estimation_method(dr) as_vs_was placebo(3)

//2. Quadratic model
did_multiplegt_stat lngca id year tau, or(2) estimation_method(dr) as_vs_was placebo(3)


*******************************************************************************/
//Panel B: First-stage effect of taxes on quantities consumed.
*******************************************************************************
//1. Linear model 
did_multiplegt_stat lngpinc id year tau, or(1)  estimation_method(dr) as_vs_was placebo(3)

//2. Quadratic model
did_multiplegt_stat lngpinc id year tau, or(2)  estimation_method(dr) as_vs_was placebo(3)

*******************************************************************************/
//IV: 
*******************************************************************************
//set trace on
did_multiplegt_stat lngca id year lngpinc tau, or(1 2 3 4 5 6 7 8)  estimator(iv-was) estimation_method(dr) placebo(3)
	
did_multiplegt_stat lngca id year lngpinc tau, or(1)  estimator(iv-was) estimation_method(dr) noextra placebo(3)

did_multiplegt_stat lngca id year lngpinc tau, or(2)  estimator(iv-was) estimation_method(dr) placebo(3)

did_multiplegt_stat lngca id year lngpinc tau, or(2)  estimator(iv-was) estimation_method(dr) noextra placebo(3)

log close

local testt gg fgg    skk gg short
local wordcount: word count `testt'

tokenize `testt'
di "`4'"
di "`wordcount'"