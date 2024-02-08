clear
qui do "This is the latest version.do"
use "data_gazoline.dta", clear

// AOSS
did_continuous lngca id year tau, or(2) estimator(aoss) estimation_method(ra) placebo noextra

// WAOSS
did_continuous lngca id year tau, or(2) estimator(waoss) estimation_method(ra) placebo noextra
did_continuous lngca id year tau, or(2) estimator(waoss) estimation_method(ps) placebo noextra
did_continuous lngca id year tau, or(2) estimator(waoss) estimation_method(dr) placebo noextra

// IWAOSS
did_continuous lngca id year lngpinc tau, or(2) estimator(iwaoss) estimation_method(ra) placebo noextra