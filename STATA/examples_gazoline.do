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

did_multiplegt_stat lngca id year tau, or(1) estimation_method(dr) as_vs_was placebo(3)

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

did_multiplegt_stat lngca id year lngpinc tau, or(1)  estimator(iv-was) estimation_method(dr) placebo(3)
	
did_multiplegt_stat lngca id year lngpinc tau, or(1)  estimator(iv-was) estimation_method(dr) noextra placebo(3)

did_multiplegt_stat lngca id year lngpinc tau, or(2)  estimator(iv-was) estimation_method(dr) placebo(3)

did_multiplegt_stat lngca id year lngpinc tau, or(2)  estimator(iv-was) estimation_method(dr) noextra placebo(3)

log close
