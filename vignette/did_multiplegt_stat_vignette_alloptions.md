
# 📘 Vignette for `did_multiplegt_stat`

> This vignette illustrates how to use the `did_multiplegt_stat` package to estimate difference-in-differences models with multiple groups and staggered treatment adoption. We demonstrate various options using the Gazoline dataset.

---

## 🔧 Setup

Before running any examples, make sure to load the program and dataset:

```stata

        ssc install did_multiplegt_stat
        net get did_multiplegt_stat
        use gazoline_did_multiplegt_stat.dta, clear

```

---

## I. 🔢 Option order: `or()` 

This option allows you to specify the polynomial orders to be used in the OLS regressions of Y_t-Y_{t-1} on a polynomial in D_{t-1} and/or in the logistic regressions of an indicator for (t-1)-to-t switchers on a polynomial in D_{t-1}.


### Example 1: For WAS and AS
```stata
did_multiplegt_stat lngca id year tau, or(1 2 3 4)
```

<img src="images/or_wasas_output.png" alt="Basic or() output" width="600"/>

---

### Example 2: For IV-WAS
```stata
did_multiplegt_stat lngca id year lngpinc tau, or(1 2 3 3 2 1 1 1) 
```

<img src="images/or_ivwas_output.png" alt="or() with controls output" width="600"/>

---

## II. 🧪 Placebo Tests

Placebo test allows checking pre-trend violations or spurious effects. This option computes the placebo version of each requested estimator. Actual estimators compare the t-1-to-t outcome evolution of period t-1-to-t switchers and stayers with the same baseline treatment. When # is equal to 1, placebo estimators (first-order placebos) compare the t-2-to-t-1 outcome evolution of period t-1-to-t switchers and stayers with the same baseline treatment, restricting attention to t-2-to-t-1 stayers. Thus, placebos assess whether switchers and stayers were on parallel trends just before switchers switched treatment. When # is strictly larger than 1, placebos comparing the outcome evolutions of t-1-to-t switchers and stayers from t-3 to t-2, from t-4 to t-3,... , and from t-#-1 to t-# are also reported, always restricting attention to stayers between those pairs of periods.

### Example 1: First-order placebos

```stata
did_multiplegt_stat lngca id year tau, placebo(1)
```

<img src="images/placebo_test1.png" alt="Placebo test results" width="600"/>

### Example 1: #-order placebos

```stata
did_multiplegt_stat lngca id year tau, placebo(3)
```

<img src="images/placebo_test2.png" alt="Placebo test results" width="600"/>

---

## III. 🔁 Switchers Analysis

Restrict estimation to switchers only:

```stata
did_multiplegt_stat lngca id year tau, switchers
```

<img src="images/switchers_results.png" alt="Switchers results" width="600"/>

---

## IV. 📈 Exact Matching Option

Match units exactly on pre-specified variables before estimating.

```stata
did_multiplegt_stat lngca id year tau, exact(id)
```

<img src="images/exact_matching.png" alt="Exact matching results" width="600"/>

---

## V. 🎲 Bootstrap Confidence Intervals

Specify the number of bootstrap iterations:

```stata
did_multiplegt_stat lngca id year tau, boot(100)
```

<img src="images/bootstrap_results.png" alt="Bootstrap results" width="600"/>

---

## VI. 🧾 Exporting Results with `esttab`

To export your results in LaTeX or text format:

```stata
esttab using results.txt, replace se
```

<img src="images/esttab_output.png" alt="esttab output" width="600"/>

---

## VII. ⚙️ Combined Example with Multiple Options

```stata
did_multiplegt_stat lngca id year lngpinc tau, ///
    or(1 2 3 4) ///
    placebo ///
    switchers ///
    exact(id) ///
    boot(50)
```

<img src="images/full_combined_output.png" alt="Full combined output" width="600"/>

---

## 📎 Appendix: File Links

- [`did_multiplegt_stat.ado`](./did_multiplegt_stat.ado)
- [`gazoline_did_multiplegt_stat.dta`](./gazoline_did_multiplegt_stat.dta)
- [Help file (`.hlp`)](./did_multiplegt_stat.hlp)

---

## ✍️ Notes

- Make sure your Stata version supports factor variable notation and the `reghdfe` dependency if required.
- This vignette is meant to be updated as more features are added.
