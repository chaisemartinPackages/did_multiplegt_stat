cap log close
log using "Results_gazoline", replace
use "gazoline_did_multiplegt_stat.dta", clear


*******************************************************************************
cap prog drop did_multiplegt_stat
qui do "did_multiplegt_stat.do"

//did_continuous lngca id year tau, or(1) estimation_method(dr) aoss_vs_waoss placebo disag

//qui do "did_multiplegt_stat_working version.do"
/******************************************************************************* R version (From the paper, version Feb 7, 2024)
*******************************************************************************/
/*
Table 2: Effects of gasoline tax on quantities consumed and prices

Panel A: Reduced-form effect of taxes on quantities consumed.
--------------------------------------------------------------------------------
                                  AOSS     s.e     WAOSS     s.e      N
                                  (1)      (2)      (3)      (4)     (5)
log(quantity) - Linear model    -0.0058  0.0026  -0.0039   0.0010   1632
log(quantity) - Quadratic model -0.0050  0.0026  -0.0038   0.0011   1632
--------------------------------------------------------------------------------
Panel B: First-stage effect of taxes on quantities consumed.
--------------------------------------------------------------------------------
                                 AOSS     s.e     WAOSS     s.e      N
                                  (1)     (2)      (3)      (4)     (5)
log(price) - Linear model       0.0028   0.0023   0.0054   0.0010   1632
log(price) - Quadratic model    0.0024   0.0024   0.0050   0.009   1632
--------------------------------------------------------------------------------

Note:All estimators in the table are computed using the data of Li et al. (2014). Columns (1) and (3) show the
AOSS and doubly-robust WAOSS estimates of the reduced-form and first-stage effects of taxes on quantities and
prices.
*/


/*******************************************************************************STATA version 
*******************************************************************************/
//Panel A: Reduced-form effect of taxes on quantities consumed.

//1. Linear model
//describe, short
set trace off
did_multiplegt_stat lngca id year tau, or(1) estimation_method(dr) aoss_vs_waoss placebo

//2. Quadratic model
did_multiplegt_stat lngca id year tau, or(2) estimation_method(dr) aoss_vs_waoss placebo


*******************************************************************************/
//Panel B: First-stage effect of taxes on quantities consumed.
*******************************************************************************
//1. Linear model 
did_multiplegt_stat lngpinc id year tau, or(1)  estimation_method(dr) aoss_vs_waoss  placebo

//2. Quadratic model
did_multiplegt_stat lngpinc id year tau, or(2)  estimation_method(dr) aoss_vs_waoss placebo

*******************************************************************************/
//IV: 
*******************************************************************************

did_multiplegt_stat lngca id year lngpinc tau, or(1)  estimator(iv-waoss) estimation_method(dr) placebo
	
did_multiplegt_stat lngca id year lngpinc tau, or(1)  estimator(iv-waoss) estimation_method(dr) noextra placebo

did_multiplegt_stat lngca id year lngpinc tau, or(2)  estimator(iv-waoss) estimation_method(dr) placebo 

did_multiplegt_stat lngca id year lngpinc tau, or(2)  estimator(iv-waoss) estimation_method(dr) noextra placebo

log close
