
# 📘 Vignette for `did_multiplegt_stat`

> This vignette illustrates how to use the `did_multiplegt_stat` package to estimate difference-in-differences models with multiple groups and staggered treatment adoption. We demonstrate various options using the Gazoline dataset.

---

## 🔧 Setup

Before running any examples, make sure to load the program and dataset:

```stata
cd "C:\path\to\your\working\directory"
cap prog drop did_multiplegt_stat
qui do "did_multiplegt_stat.ado"

use "gazoline_did_multiplegt_stat.dta", clear
```

---

## I. 🔢 `or()` Option with Multiple Inputs

This option allows you to specify multiple treatment intensities.

### Example 1: Basic `or()` usage
```stata
did_multiplegt_stat lngca id year tau, or(1 2 3 4)
```

<img src="images/or_basic_output.png" alt="Basic or() output" width="600"/>

---

### Example 2: Including Controls
```stata
did_multiplegt_stat lngca id year lngpinc tau, or(1 2 3 4 5 6 7 8)
```

<img src="images/or_with_controls.png" alt="or() with controls output" width="600"/>

---

## II. 🧪 Placebo Tests

Placebo test allows checking pre-trend violations or spurious effects.

```stata
did_multiplegt_stat lngca id year tau, placebo
```

<img src="images/placebo_test.png" alt="Placebo test results" width="600"/>

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
